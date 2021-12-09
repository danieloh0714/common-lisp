(in-package :sddr-tests)

(defparameter *member-kb*
  '(
    (<- (member ?x (cons ?x ?y)))
    (<- (member ?x (cons ?z ?y))
        (member ?x ?y))
    ))
