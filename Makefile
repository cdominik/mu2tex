

README.pod: mu2tex.el
	perl -ne 'if (/^;;; Commentary/../^;;;;;;;;;/) {s/^;;;? ?//;print}' mu2tex.el > README
