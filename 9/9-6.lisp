(defun horner (x &rest coefficients)
  (reduce #'(lambda (coefficients1 coefficients2)
              (+ (* coefficients1 x) coefficients2))
          coefficients))
