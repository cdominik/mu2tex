;;; mu2tex.el --- Convert plain text molecule names and units to TeX
;; Copyright (c) 2005, 2019 Carsten Dominik

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Version: 1.3
;; Keywords: tex
;; URL: https://github.com/cdominik/mu2tex

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;---------------------------------------------------------------------------
;;
;;; Commentary:
;;
;; In scientific literature, it is common to format the symbols for
;; chemical elements, molecules and unit names by using roman (as
;; opposed to italic) letters for the names.  This makes such things
;; cumbersome to type, because one would have to do either
;; $\mathrm{H_2O}$ or H$_2$O.  This package make this typing easier by
;; providing the function mu2tex.  This function converts plain
;; ASCII molecule names and unit expressions into either of the above
;; forms.  It also makes it easier to write numbers times powers of 10.
;;
;; INSTALLATION
;; ------------
;; Put this file on your load path, byte compile it (not required),
;; and copy the following code into your .emacs file.  Change the key
;; definition to your liking.
;;
;;   (autoload 'mu2tex "mu2tex"
;;     "Insert constants into source code" t)
;;   (define-key global-map "\C-cm" 'mu2tex)
;;
;; USAGE
;; -----
;;
;; To use this package, just type the name of a molecule or a unit
;; expression into the buffer.  The expression must not contain
;; spaces.  Then, with the cursor right after the types expression,
;; press the key to which mu2tex has been assigned.
;;
;; Example              Conversion result
;; -----------------    -----------------
;; H2O                  H$_2$O
;; C18O                 C$^18$O
;; H2C17O               H$_2$C$^17$O
;; m2s-2                m$^2$ s$^{-2}
;; 2.74e-13             $2.74\times10^{-13}$
;;
;; A dot can be used as a separator where one is needed:
;;
;; H2.18O               H$_2${}$^{18}$O
;; erg.cm-2s-1          erg cm$^{-2}$ s$^-1$
;; 2.7e-2Jy.arcsec-2    $2.7\times10^{-2}$ Jy arcsec$^-2$
;;
;; The dot can also be used to force interpretation of a number as either
;; isotope number or stoichiometric number.  By default, mu2tex assumes
;; that numbers smaller than 10 are stoichiometric coefficients that need
;; to be turned into a subscript, and numbers 10 and larger are isotope
;; numbers that need to become superscripts.  In the cases where this
;; heuristics fails, you can use the dot to force assignment to left or
;; right and in this way break the ambiguity:
;;
;; H.6Li                H{}$^{6}$Li
;; C18.H                C$_{18}$H
;;
;; Inside (La)TeX math mode, a different conversion is needed.  In this
;; case, the resulting string should be put into a \mathrm macro, to get
;; the correct fonts, and no $ characters are needed to swith mode.
;; If you have the texmathp.el package, recognition of the environment will
;; be automatic.  If you don't, you can use the command `mu2tex-math' to
;; get the math-mode version of the conversion.
;;
;; CUSTOMIZATION
;; -------------
;; You can use the following variables to customize this mode:
;;
;; mu2tex-use-mathrm
;;    Do you prefer $\mathrm{H_2O}$ or H$_2$O in math mode?
;;
;; mu2tex-use-texmathp
;;    Should texmathp.el be used to determine math or text mode?
;;
;; mu2tex-isotope-limit
;;    Numbers larger than or equal to this are interpreted as isotope numbers.
;;
;; mu2tex-molecule-exceptions
;;    Special molecules for which the automatic converter fails.
;;
;; mu2tex-units
;;    Units that can be used to distinguish a unit expression from a molecule.
;;
;; mu2tex-space-string
;;    What should be inserted as separator between different units?
;;
;; Heuristics, and fixes where needed
;; ----------------------------------
;; - Mu2tex uses a heuristic method to decide if the expression to convert
;;   is a unit expression or a molecule name.  This method is based on a
;;   built-in list of unit names that never show up in molecule names.
;;   This list (stored in the constant `mu2tex-default-units') may or may not
;;   work for you.  See the variable `mu2tex-units'.  Also, an initial
;;   floating point number containing a decimal point or exponent does force
;;   unit interpretation.  You can call `mu2tex' with single C-u prefix to
;;   force interpretation as a molecule name.  A double prefix C-c C-u enforces
;;   interpretation as a unit expression.  Finally, you can also use the
;;   special commands `mu2tex-unit' and `mu2tex-molecule'.
;;
;; - If you have specific molecules that you need often and that defy the
;;   heuristics for isotope numbers and stoichiometric coefficients, you
;;   can simply hard-code the conversion for these molecules using the
;;   variable `mu2tex-molecule-exceptions'.  For example:
;;
;;        (setq mu2tex-exceptions
;;              '((\"HC18H\" . \"HC$_{18}$H\")
;;                (\"H6Li\"  . \"H$^{6}$Li\")))
;;
;;
;; AUTHOR
;; ------
;; Carsten Dominik <dominik@.uva.nl>
;;
;; ACKNOWLEDGEMENTS
;; ----------------
;; Cecilia Ceccarelli made me write papers about chemistry, and in this way
;; prompted this program.  She also had the idea for the unit formatter.
;;
;; CHANGES
;; -------
;; Version 1.0
;; - Initial release
;; Version 1.1
;; - Add formatting of numbers
;; Version 1.2
;; - Allow point to force isotope/stoichiometric assignment like C18.H
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar mu2tex-molecule-chars "-+a-zA-Z0-9()/.\\\\*"
  "Characters allowed in a molecule name before formatting.")

(defcustom mu2tex-use-mathrm nil
  "Non-nil means, wrap the entire entry into \\mathrm instead of using $."
  :group 'mu2tex
  :type 'boolean)

(defcustom mu2tex-use-texmathp t
  "Non-nil means, try to use the texmathp.el package if available.
When nil, text mode is assumed, and you need to use the command `mu2tex-math'
to get the math-mode conversion."
  :group 'mu2tex
  :type 'boolean)

(defcustom mu2tex-isotope-limit 10
  "Numbers greater or equal than this are interpreted as isotope numbers.
These will be raised as superscripts.  Numbers smaller than this will be
interpreted as stoichiometric coefficients and be lowered as subscripts."
  :group 'mu2tex
  :type 'integer)

(defcustom mu2tex-molecule-exceptions nil
  "List of exceptions for the molecule formatter.
The formatting follows a set of simple rules, see the command `mu2tex'.
For some molecules, the rules may not lead to the correct result.
This variable is a list of cons cells where the key is a molecule as typed
in plain ASCII, and the cdr is the translation into TeX.  The translation
should contain $ where necessary to make it work outside math mode - inside
math mode, these $'s will be removed automatically.
For example, HC18H will be formatted incorrectly to HC$^{18}$H, because the
rules assume that the number 18 indicates an isope identifier.  To make sure
also this molecule is formatted correctly, use

   (setq mu2tex-exceptions '((\"HC18H\" . \"HC$_{18}$H\")))"
  :group 'mu2tex
  :type '(repeat (cons
                  (string :tag "Typed string")
                  (string :tag "Replacement"))))

(defconst mu2tex-default-units
  '("m" "cm" "km" "mm" "nm" "pc" "kpc" "Mpc" "AU" "lj"
    "s" "min" "hr" "d" "wk" "yr" "Myr" "Gyr" "a" "Ma" "Ga" "Hz"
    "g" "kg" "t"
    "dyn"  ;; Cannot use "N" because that is of course Nitrogen.
    "J" "erg" "cal" "eV" "mK"
    "mol"
    "Pa" "hPa" "atm" "bar"
    "Bq" "Ci" "rem" "Jy"
    "rad" "sr" "arcmin" "arcsec" "deg" "degree")
  "Words that uniquely identify a unit expression.  Case is significant.
This default value is good for astronomy papers.")

(defcustom mu2tex-units
  '("Jy" "lj" "AU"
    ("um" . "$\\mu$m")
    ("Ls" . "$L_\\odot$")
    ("Ms" . "$M_\\odot$")
    ("Rs" . "$R_\\odot$")
    ("Me" . "$M_\\oplus$")
    ("Re" . "$R_\\oplus$")
    ("L*" . "$L_\\star$")
    ("R*" . "$R_\\star$")
    ("M*" . "$M_\\star$")
    )
  "Units names that can distinguish a unit expression from a molecule.
In the basic setup, this is just a list of strings.  If any of these strings
is found in an expression handled by `mu2tex', it triggers interpretation
as a unit expression instead of a molecule name.  This list enhances the one
given in the constant `mu2tex-default-units', unless the string \"NODEFAULTS\"
is part of this list.

As a special treat, each of the entries in this list can also be a cons cell
where the car specifies the unit name, and the cdr a replacement text that
should be inserted instead.  I use this for units containing TeX macros, like
the examples given in the default value of `mu2tex-units'."
  :group 'mu2tex
  :type '(repeat (choice
		  (string :tag "Unit Name")
		  (cons (string :tag "Unit Name  ")
			(string :tag "Replacement")))))

;;;###autoload
(defun mu2tex-molecule ()
  "Call `mu2tex', enforcing interpretation as a molecule name."
  (mu2tex 'molecule))

;;;###autoload
(defun mu2tex-unit ()
  "Call `mu2tex', enforcing interpretation as a unit expression."
  (mu2tex 'unit))

;;;###autoload
(defun mu2tex-math (arg)
  "Call `mu2tex' enforcing math mode, passing ARG through to `mu2tex'."
  (interactive "P")
  (mu2tex arg 'force-math))

;;;###autoload
(defun mu2tex (&optional arg force-math)
  "Formats a plain ASCII molecule name or unit expression ARG for TeX.
This command grabs a molecule name or unit expression *before the cursor*
and inserts the proper sequences to make it look good when typeset with TeX.
The string taken from the buffer must be continuous, without spaces.  A dot
can be inserted as separator when this is needed to make things unique.

Examples:
   H2O                  =>  H$_2$O
   C18O                 =>  C$^18$O
   H2C17O               =>  H$_2$C$^17$O
   erg.cm-2s-1          =>  erg cm$^{-2}$ s$^-1$
   2.7e-2Jy arcsec-2    =>  $2.7\times10^{-2}$ Jy arcsec$^-2$

   H2.18O               =>  H$_2${}$^{18}$O
   22.5kg.m.s-2         =>  22.5 kg m s$^{-2}$

Assumptions:
1. When converting a molecule name:
   The program assumes that the characters A\-Z,a-z,0-9,+,-,(,) can be
   part of the name of a molecule.  + and - will be interpreted as
   charges and put into a superscript.  Numbers smaller than 10 will be
   interpreted as stoichiometric coefficients and will be formatted as
   subscripts.  Numbers 10 and up will be interpreted as isotope
   indications and will be raised.  Letters will be left alone, and so
   will be leading ortho- and para- (or o- and p-) prefixes.
2. When converting a unit expression
   The program assumes that a numeric value only occurs at the beginning.

The decision as to whether a texpression before point is a unit expression or
a molecule name is heuristic.  You can enforce interpretation as a molecule
name with one `C-u' prefix, and interpretations as a unit expression with
two `C-u C-u' prefixes.

When called from Lisp, FORCE-MATH indicates that the conversion should
be done for math mode."
  (interactive "P")
  (let* ((end (point))
         (beg (mu2tex-beginning-of-entry))
         (raw (buffer-substring beg end))
	 (up (mu2tex-unit-p raw))
	 (unitp (cond ((or (eq arg 'molecule) (equal arg '(4))) nil)
		      ((or (eq arg 'unit) (equal arg '(16))) t)
		      ((or (and up (not arg)) (and (not up) arg)) t)
		      (t nil)))
         (new (if unitp
		  (mu2tex-format-unit raw)
		(mu2tex-format-molecule raw)))
	 tmp)
    (delete-region beg end)
    (goto-char beg)
    (when (or (setq tmp (or force-math (mu2tex-mathp)))
	      mu2tex-use-mathrm)
      (while (string-match "\\$+" new) ; need to get rid of dollars
	(if (save-match-data
	      (string-match "\\\\[a-zA-Z]+$"
			    (substring new 0 (match-beginning 0))))
	    (setq new (replace-match "{}" t t new))
	  (setq new (replace-match "" t t new))))
      (while (string-match " " new)    ; need to replace spaces
	(setq new (replace-match "\\," t t new)))
      (setq new (concat "\\mathrm{" new "}")) ; wrap in \mathrm
      (if (not tmp) (setq new (concat "$" new "$"))))
    (insert new)
    (message "Formatted as a %s" (if unitp "unit expression" "molecule name"))))

(defun mu2tex-mathp ()
  "Is point in math mode?
This used either `texmathp' for sophisticated parsing, or simply checks if
point is inside one of the standard math environments."
  (if (and (fboundp 'texmathp) mu2tex-use-texmathp)
      (texmathp)
    nil))

(defun mu2tex-beginning-of-entry ()
  "Return startposition of molecule name or unit expression."
  (max
   (save-excursion (skip-chars-backward mu2tex-molecule-chars) (point))
   (save-excursion (condition-case nil
                       (progn (up-list -1) (1+ (point)))
                     (error 1)))))

(defun mu2tex-unit-re ()
  "Return a regular expression matching a unit expression."
  (concat "\\(\\`\\|[^a-zA-Z]\\)"
	  "\\("
	  (mapconcat 'identity
		     (append
		      (if (member "NODEFAULTS" mu2tex-units)
			  nil
			mu2tex-default-units)
		      (mapcar (lambda (x) (if (consp x) (regexp-quote (car x)) x))
			      mu2tex-units))
		     "\\|")
	  "\\)"
	  "\\(\\'\\|[^a-zA-Z]\\)"))
	  
(defun mu2tex-unit-p (s)
  "Is S a unit (not a molecule name)?"
  (if (member s mu2tex-molecule-exceptions) ;; must be a molecule
      nil
    (or (string-match "^[0-9]+\\(\\.\\|[xe][-+]?[0-9]\\)" s) ; initial float
	(let (case-fold-search) (string-match (mu2tex-unit-re) s)))))

(defun mu2tex-format-molecule (string)
  "Formats the molecule name STRING."
  (let ((rest string) (molec "")
        (case-fold-search nil)
        (re (concat
             "^"
             "\\(o-\\|p-\\|[Oo]rtho-\\|[pP]ara-\\|[a-zA-HJ-Z]+\\)"
             "\\|\\([-+]+\\)"
             "\\|\\([0-9]+\\)"
             "\\|\\( \\)"
             "\\|\\(\\.\\)"
             "\\|\\(.\\)"))
	s l n last-was-dot lwdot nidot)
    (if (assoc rest mu2tex-molecule-exceptions)
        (setq molec (cdr (assoc rest mu2tex-molecule-exceptions)))
      (while (not (string= rest ""))
        (if (not (string-match re rest))
            (setq molec (concat molec rest) rest "")
          (setq s (match-string 0 rest)
                l (length s)
		n (string-to-number s)
                rest (substring rest l)
		lwdot last-was-dot
		nidot (equal (string-to-char rest) ?.)
		last-was-dot nil)
          (setq molec
                (concat molec
                        (cond
                         ((match-end 1) s)
                         ((match-end 2) ; charge
                          (if (> l 1)
                              (concat "$^{" s "}$")
                            (concat "$^" s "$")))
                         ((match-end 3) ; number
			  (if (> l 1) (setq s (concat "{" s "}")))
			  (cond
			   ((equal molec "") (concat "$^" s "$"))
			   ((and (or lwdot
				     (>= n mu2tex-isotope-limit))
				 (not nidot))
			    (concat "$^" s "$"))
			   (t (concat "$_" s "$"))))
                         ((match-end 4) "")
                         ((match-end 5)
			  (setq last-was-dot t) "{}")
                         ((match-end 6) s)
                         )))
	  )))
    ;; remove double dollars
    (while (string-match "\\$\\$" molec)
      (setq molec (replace-match "" t t molec)))
    molec))

(defun mu2tex-format-unit (string)
  "Formats the unit expression STRING."
  (let ((rest string) (unit "") m e p u)
    ;; Check for and convert number
    (when (string-match "^\\([-+]?[0-9.]+\\)\\([ex]\\([-+]?[0-9]+\\)\\)?" rest)
      (setq m (match-string 1 rest)
	    e (if (match-beginning 2) (match-string 3 rest))
	    rest (substring rest (match-end 0))
	    unit (concat (if e "$" "")
			 m
			 (if e (concat "\\times10^{" e "}") "")
			 (if e "$" "")
			 ))
      ;; Enforce space after number
      (or (member (string-to-char rest) '(?. ?/ ?\ ))
	  (setq rest (concat "." rest))))
    ;; Find and convert units and powers
    (while (string-match "^\\([./]?\\)\\([\\a-zA-Z]+\\*?\\)\\([-+]?[0-9]+\\)?" rest)
      (setq p (mu2tex-space (match-string 1 rest))
            u (match-string 2 rest)
	    u (or (cdr (assoc u mu2tex-units)) u)
	    e (if (match-beginning 3) (match-string 3 rest))
	    rest (substring rest (match-end 0))
	    unit (concat unit
			 (if (equal unit "") "" p)
			 u
			 (if e
			     (if (> (length e) 1)
				 (concat "$^{" e "}$")
			       (concat "$^" e "$"))
			   ""))))
    unit))

(defcustom mu2tex-space-string " "
  "The type of space between two units that should be inserted.
Useful values are for example \" \", \"~\", \"\\ \", or \"\\,\"."
  :group 'mu2tex
  :type 'string)

(defun mu2tex-space (S)
  "Translates separators in S."
  (cond
   ((equal S "")  mu2tex-space-string)
   ((equal S ".") mu2tex-space-string)
   ((equal S "/") "/")))

;; Try to find texmathp.el avalable.
(condition-case nil (require 'texmathp) (error nil))

(provide 'mu2tex)

;;; mu2tex.el ends here

