;;;; A stack-based esoteric programming language named "flush"
(ql:quickload 'cl-ppcre)

(defpackage :flush
  (:use :common-lisp))

(in-package :flush)

(defvar *var-table* (make-hash-table)
  "Holds all builtin flush functions and variables")

(defun tokenize (code)
  "Break code string into a list of string tokens"
  (declare (string code))
  (cl-ppcre:all-matches-as-strings
    "-?[\\d\\.]+|\".*?[^\\\\]\"|\\w+|[^ \\t\\n]"
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

(defun run (code &optional (starting-stack ()))
  "Run CODE as a flush program, returning the final stack value"
  (declare (string code))
  (let ((stack starting-stack))
    (dolist (token (tokenize code))
      (cond
        ((literalp token)
         (push (read-flush-literal token) stack))
        ((char= (elt token 0) #\;)
         (setf stack (nreverse stack))
         (setf stack (list (apply (gethash (car stack) *var-table*)
                                  (cdr stack)))))
        ((char= (elt token 0) #\@)
         (setf stack (append (cdr stack) (list (car stack)))))
        ((gethash (intern token) *var-table*)
         (push (intern token) stack))
        (t
         (error "Unrecognized token ~S"
                token))))
    stack))

(defun repl ()
  "Start a REPL for flush. QUIT exits"
  (loop with stack-value = ()
        for flush-string = (progn
                             (format t "> ")
                             (read-line)) do
        (if (string= flush-string "QUIT")
          (return)
          (let ((ending-stack
                  (run (concatenate 'string
                                    flush-string
                                    (string #\Newline))
                       stack-value))) 
            (format t "~A~%"
                    (reverse ending-stack))
            (setf stack-value ending-stack)))))

;;; Export all symbols for unit testing
(let ((flush (find-package :flush)))
  (do-all-symbols (symbol flush)
    (when (eql (symbol-package symbol)
               flush)
      (export symbol))))
