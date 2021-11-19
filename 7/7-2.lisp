(defun map-stream (fn stream)
  (let ((eof (list nil)))
    (do ((exp (read stream nil eof) (read stream nil eof)))
      ((eql exp eof) nil)
      (funcall fn exp))))

(defun map-file (fn pathname)
  (with-open-file (stream pathname :direction :input)
    (map-stream fn stream)))
