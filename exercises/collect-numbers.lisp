(defun collect-numbers (s-exp)
  (cond ((numberp s-exp) (list s-exp))
        ((atom s-exp) nil)
        (t (mapcan (lambda (el) (collect-numbers el)) s-exp))))
