(defclass tokenizer ()
  ((str :accessor tr-str
        :initarg :str)
   (delim :accessor tr-delim
          :initarg :delim)
   (index :accessor tr-index
          :initform 0)))

(defmethod next-token-p ((tr tokenizer))
  (tr-index tr))

(defmethod next-token-index ((tr tokenizer) start)
  (if (eql (tr-delim tr) #\space)
    (position-if-not #'(lambda (c) (eql c (tr-delim tr))) (tr-str tr) :start start)
    start))

(defmethod next-token ((tr tokenizer))
  (let* ((end (position (tr-delim tr) (tr-str tr) :start (tr-index tr)))
         (res (subseq (tr-str tr) (tr-index tr) end)))
    (setf (tr-index tr) (and end (next-token-index tr (1+ end))))
    res))

(defun make-tokenizer (str delim)
  (let ((tr (make-instance 'tokenizer :str str :delim delim)))
    (setf (tr-index tr) (next-token-index tr 0))
    tr))

(defun split-string (str &optional (delim #\space))
  (let ((tr (make-tokenizer str delim)))
    (do ((l nil (cons (next-token tr) l)))
      ((not (next-token-p tr)) (nreverse l)))))
