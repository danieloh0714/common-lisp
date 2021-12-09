(defpackage trie
  (:use :common-lisp)
  (:export trie make-trie add-word subtrie trie-word trie-count mapc-trie read-words))

(in-package trie)

(defstruct (trie (:print-function print-trie))
  (word nil)
  (count 0)
  (branches nil))

(defun print-trie (node stream depth)
  (format stream "#<~S, ~S subwords>" (trie-word node) (trie-count node)))

(defun traverse (char trie add)
  (let ((branches (assoc char (trie-branches trie))))
    (cond ((or (not add) branches) (cdr branches))
          (t (let ((new-branch (make-trie)))
               (push (cons char new-branch) (trie-branches trie))
               new-branch)))))

(defun add-word (word trie &key (depth 0) (len (length word)))
  (incf (trie-count trie))
  (cond ((= depth len) (setf (trie-word trie) word) trie)
        (t (add-word word (traverse (char word depth) trie t) :depth (1+ depth) :len len))))

(defun subtrie (trie &rest chars)
  (do ((keys chars (cdr keys))
       (subtrie trie (and subtrie (traverse (car keys) subtrie nil))))
    ((null keys) subtrie)))

(defun mapc-trie (fn trie)
  (let ((branches (trie-branches trie)))
    (mapc #'(lambda (branch) (funcall fn (car branch) (cdr branch))) branches)
    trie))

(defun read-words (file trie)
  (with-open-file (stream file)
    (do ((l (read-line stream nil) (read-line stream nil)))
      ((null l) trie)
      (add-word l trie))))
