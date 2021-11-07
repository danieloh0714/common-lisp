(defun longer-list (lst-one lst-two)
  (if (> (length lst-one) (length lst-two))
      lst-one
      lst-two))

(defun new-path (end path node net)
  (cond ((eql node end) (cons node path))
        ((member node path) nil)
        (t (dfs end (cons node path) net))))

(defun dfs (end curr net)
  (do ((neighbors (cdr (assoc (car curr) net)) (cdr neighbors))
       (longest nil (longer-list longest (new-path end curr (car neighbors) net))))
    ((null neighbors) longest)))

(defun longest-path (start end net)
  (or (reverse (dfs end (list start) net))
      (if (eql start end)
          (list start)
          nil)))
