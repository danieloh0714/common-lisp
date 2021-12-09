(defun handle-str-vals (str start &optional (res str))
  (let ((quot-start (position #\" str :start start)))
    (if quot-start
      (let ((quot-end (position #\" str :start (1+ quot-start))))
        (handle-str-vals str (1+ quot-end) (nsubstitute #\_ #\space str :start quot-start :end quot-end)))
      res)))

(defun symbolize (str)
  (if (ignore-errors (numberp (read-from-string str)))
    (parse-integer str)
    (intern (string-upcase str))))

(defun add-token (str res)
  (if (string= "" str) res (cons (symbolize str) res)))

(defun space-equiv-p (char)
  (or (char= #\space char)
      (char= #\tab char)
      (char= #\newline char)
      (char= #\, char)))

(defun next-token-end (str i)
  (do ((j i (1+ j)))
    ((or (>= j (length str)) (space-equiv-p (char str j))) j)))

(defun atomize (str)
  (let ((s (remove #\" (handle-str-vals str 0))))
    (do ((res nil (add-token (subseq s curr next) res))
         (curr 0 (1+ next))
         (next (next-token-end s 0) (next-token-end s (1+ next))))
      ((>= curr (length s)) (reverse res)))))
