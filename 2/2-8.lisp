;; a iterative
(defun print-dots (n)
  (do ((i n (1- i)))
    ((= i 0) nil)
    (format t ".")))

;; a recursive
(defun print-dots (n)
  (unless (= n 0)
    (format t ".")
    (print-dots (1- n))))

;; b iterative
(defun get-a-count (lst)
  (do ((ll lst (cdr ll))
       (count 0
              (if (eql (car ll) 'a)
                  (1+ count)
                  count)))
    ((null ll) count)))

;; b recursive
(defun get-a-count (lst)
  (cond ((null lst) 0)
        ((eql 'a (car lst)) (1+ (get-a-count (cdr lst))))
        (t (get-a-count (cdr lst)))))
