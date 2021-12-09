(defun has-number-p (s-exp)
  (if (not (listp s-exp))
      (numberp s-exp)
      (some #'has-number-p s-exp)))
