(in-package #:exmatch)

(defun ?contains (x y lsts)
  (if (atom y)
    (match-p (car x) y lsts)
    (append (mapcan #'(lambda (y-val) (?contains x y-val lsts)) y)
            (match-p (car x) y lsts))))
