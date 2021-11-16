(defun max-min (vec &key (start 0) (end (length vec)) (max-el nil) (min-el nil))
  (if (= start end)
    (values max-el min-el)
    (let ((temp (svref vec start)))
      (max-min vec
               :start (1+ start)
               :end end
               :max-el (max temp (or max-el temp))
               :min-el (min temp (or min-el temp))))))
