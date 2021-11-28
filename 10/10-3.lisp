(defmacro nth-expr (n &rest expressions)
  `(case ,n
     ,@(loop for i in expressions
             for n from 1
             collect `((,n) ,i))))
