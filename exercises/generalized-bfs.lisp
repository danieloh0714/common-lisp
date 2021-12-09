(defun new-paths (path neighbors)
  (mapcan #'(lambda (n)
              (if (member n path)
                nil
                (list (cons n path))))
          neighbors))

(defun bfs (paths pred gen)
  (if (empty-queue-p paths)
    nil
    (let* ((neighbors (funcall gen (car paths)))
           (final (find-if pred neighbors)))
      (cond (final (cons final (car paths)))
            (t (bfs (append (cdr paths) (new-paths (car paths) neighbors)) pred gen))))))

(defun shortest-path (start end net)
  (reverse (bfs (list (list start))
                (lambda (x) (eql x end))
                (lambda (path) (cdr (assoc (car path) net))))))
