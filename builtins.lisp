;;;; Builtin functions for the flush language

(in-package :flush)

(defmacro define-flush-fun (name &body body)
  "Define a new builtin flush function
   Binds ARGS to the list of args passed to the function"
  `(setf (gethash ',name *var-table*)
     (lambda (&rest args)
       ,@body)))

(define-flush-fun +
  (apply #'+ args))

(define-flush-fun -
  (apply #'- args))

(define-flush-fun *
  (apply #'* args))

(define-flush-fun /
  (apply #'/ args))

(define-flush-fun %
  (mod (car args) (cadr args)))

(define-flush-fun |map|
  (mapcar (car args) (cadr args)))

(define-flush-fun |range|
  (if (= (length args) 2)
    (loop for n from (car args) to (cadr args) collecting n)
    (loop for n to (car args) collecting n)))
