;; a
(defun summit (lst)
  (apply #'+ (remove nil lst)))

;; b
(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst)))))))
