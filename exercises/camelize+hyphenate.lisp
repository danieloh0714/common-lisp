(defun camelize (str &optional capitalize)
  (let ((res (remove #\- (string-capitalize str))))
    (if capitalize res (nstring-downcase res :end 1))))

(defun case-change-p (str i)
  (and (> i 0)
       (lower-case-p (char str (1- i)))
       (upper-case-p (char str i))))

(defun camel-to-hyphenated (str stream format-str)
  (do ((end (length str))
       (i 0 (1+ i)))
    ((= i end))
    (when (case-change-p str i)
      (write-string "-" stream))
    (format stream format-str (char str i))))

(defun hyphenate (str &optional (case :upper))
  (with-output-to-string (stream)
    (camel-to-hyphenated str stream (ecase case
                                      ((:upper) "~:@(~a~)")
                                      ((:lower) "~(~a~)")))))
