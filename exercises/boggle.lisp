(defpackage boggle
  (:use :common-lisp trie)
  (:export solve-boggle load-words))

(in-package boggle)

(defvar *trie* (make-trie))

(defun get-adj (board row col)
  (let ((n (isqrt (length board))))
    (loop for r from (1- row) to (1+ row)
          append (loop for c from (1- col) to (1+ col)
                       when (and (or (/= r row) (/= c col))
                                 (< -1 r n) (< -1 c n))
                       append (list (cons r c))))))

(defun get-adj-list (board)
  (let ((n (isqrt (length board))))
    (loop for row from 0 to (1- n)
          append (loop for col from 0 to (1- n)
                       append (list (get-adj board row col))))))

(defun get-subtrie (trie board row col)
  (let ((l (elt board (+ (* (isqrt (length board)) row) col))))
    (if (eql #\q l)
      (subtrie trie #\q #\u)
      (subtrie trie l))))

(defun find-word (trie board adj-list row col &optional (seen nil))
  (let ((trie (get-subtrie trie board row col)))
    (if (null trie)
      nil
      (let* ((neighbors (elt adj-list (+ (* (isqrt (length adj-list)) row) col)))
             (words (mapcan (lambda (neighbor)
                              (unless (member neighbor seen :test #'equal)
                                (find-word trie board adj-list (car neighbor) (cdr neighbor) (acons row col seen))))
                            neighbors)))
        (when (>= (length (trie-word trie)) 3)
          (push (trie-word trie) words))
        words))))

(defun solve-boggle (board)
  (let* ((n (isqrt (length board)))
         (adj-list (get-adj-list board))
         (words (loop for row from 0 to (1- n)
                      append (loop for col from 0 to (1- n)
                                   append (find-word *trie* board adj-list row col)))))
    (sort (sort (remove-duplicates words) #'string<) #'> :key #'length)))
    
(defun load-words (pathname)
  (with-open-file (stream pathname)
    (do ((l (read-line stream nil) (read-line stream nil)))
      ((null l))
      (add-word l *trie*))))
