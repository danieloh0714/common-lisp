;;; RING
(defpackage "RING"
  (:use "COMMON-LISP")
  (:export "BUF-CLEAR" "BUF-FLUSH" "BUF-INSERT" "BUF-NEXT" "BUF-POP" "BUF-RESET" "NEW-BUF"))

(in-package "RING")

;;; FILE
(defpackage "FILE"
  (:use "COMMON-LISP")
  (:export "FILE-SUBST" "STREAM-SUBST"))

(in-package "FILE")
(use-package "RING")
