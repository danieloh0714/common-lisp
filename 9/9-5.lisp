(defun solver (f min max epsilon)
  (let ((i (/ (+ min max) 2)))
    (cond ((<= (abs (- max min)) epsilon) max)
          ((< (funcall f i) 0) (solver f i max epsilon))
          (t (solver f min i epsilon)))))

(defun solve (f min max epsilon)
  (if (< (funcall f min) 0)
    (solver f min max epsilon)
    (solver f max min epsilon)))
