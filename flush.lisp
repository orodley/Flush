;;;; A stack-based esoteric programming language named "flush"
(ql:quickload 'cl-ppcre)

(defpackage :flush
  (:use :common-lisp))

(in-package :flush)

(defun tokenize (code)
  "Break code string into a list of string tokens"
  (declare (string code))
  (cl-ppcre:all-matches-as-strings
    "-?[\\d.]+|\".*?[^\\\\]\""
    code))

(defun literalp (token)
  (declare (string token))
  (let ((first-char (elt token 0)))
    (or (member first-char '(#\- #\" #\.))
        (digit-char-p first-char))))

(defun read-flush-literal (token)
  "Read a flush literal from a string and return the value it represents"
  (declare (string token))
  (read-from-string token))

(defun run (code)
  "Run CODE as a flush program, returning the final stack value"
  (declare (string code))
  (let ((stack ()))
    (dolist (token (tokenize code))
      (cond
        ((literalp token)
         (push (read-flush-literal token) stack))
        (t
         (error "Unrecognized token ~S"
                token))))
    stack))

;;; Export all symbols for unit testing
(let ((flush (find-package :flush)))
  (do-all-symbols (symbol flush)
    (when (eql (symbol-package symbol)
               flush)
      (export symbol))))
