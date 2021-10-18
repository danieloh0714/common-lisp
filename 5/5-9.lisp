;; a
(defun new-paths (path node end net)
  (mapcan #'(lambda (n)
              (cond ((eql n end) (throw 'abort (reverse (cons n path))))
                    ((member n path) nil)
                    (t (list (cons n path)))))
          (cdr (assoc node net))))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
      (let* ((path (car queue)) (node (car path)))
        (bfs end
             (append (cdr queue)
                     (new-paths path node end net))
             net))))

(defun shortest-path (start end net)
  (catch 'abort (bfs end (list (list start)) net)))

;; b
(defun new-paths (path neighbors)
  (mapcan #'(lambda (n)
              (if (member n path)
                  nil
                  (list (cons n path))))
          neighbors))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
      (let* ((path (car queue)) (node (car path)) (neighbors (cdr (assoc node net))))
        (if (member end neighbors)
            (reverse (cons end path))
            (bfs end
                 (append (cdr queue) (new-paths path neighbors))
                 net)))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
