;; a
(defun new-paths (path node end net)
  (mapcar #'(lambda (n)
              (let ((new-path (cons n path)))
                (cond ((eql n end) (throw 'abort (reverse new-path)))
                      ((member n path) nil)
                      (t new-path))))
          (cdr (assoc node net))))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (bfs end
               (append (cdr queue)
                       (new-paths path node end net))
               net)))))

(defun shortest-path (start end net)
  (catch 'abort (bfs end (list (list start)) net)))

;; b
(defun new-paths (path node end net)
  (mapcar #'(lambda (n)
              (let ((new-path (cons n path)))
                (cond ((eql n end) (return-from new-paths new-path))
                      ((member n path) nil)
                      (t new-path))))
          (cdr (assoc node net))))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (let ((new-path (new-paths path node end net)))
            (if (eql end (car new-path))
                (reverse new-path)
                (bfs end
                     (append (cdr queue)
                             (new-paths path node end net))
                     net)))))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
