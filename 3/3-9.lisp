(defun is-longer-path (path-one path-two)
  (if (>= (length path-one) (length path-two))
      path-one
      path-two))

(defun dfs (start node end net)
  (cond ((eql node end) (list start end))
        ((null (longest-path node end (remove (assoc start net) net))) nil)
        (t (cons start (longest-path node end (remove (assoc start net) net))))))

(defun longest-path (start end net)
  (do ((neighbors (cdr (assoc start net)) (cdr neighbors))
       (curr-path (if (not (eql start end))
                       nil
                       (list start))
                  (is-longer-path curr-path (dfs start (car neighbors) end net))))
    ((null neighbors) curr-path)))
