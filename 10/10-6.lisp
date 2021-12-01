(defmacro preserve (vars &body body)
  `((lambda ,vars ,@body) ,@vars))
