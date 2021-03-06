;;;; Tests
(ql:quickload 'lisp-unit)
(defpackage :flush-tests
  (:use :common-lisp :flush :lisp-unit))

(in-package :flush-tests)

;;; TOKENIZE tests
(define-test tokenize-tokenizes-ints
  (assert-equal
    '("1" "2" "3")
    (tokenize "1 2 3")))

(define-test tokenize-tokenizes-negative-ints
  (assert-equal
    '("-1" "-2")
    (tokenize "-1 -2")))

(define-test tokenize-tokenizes-floats
  (assert-equal
    '("1.2" "3.4")
    (tokenize "1.2 3.4")))

(define-test tokenize-tokenizes-floats-with-no-leading-digit
  (assert-equal
    '(".1" ".2")
    (tokenize ".1 .2")))

(define-test tokenize-tokenizes-symbols-without-spaces
  (assert-equal
    '("+" "@")
    (tokenize "+@")))

(define-test tokenize-tokenizes-lists
  (assert-equal
    '("( 1 2 3 )")
    (tokenize "(1 2 3)")))

(define-test tokenize-tokenizes-nested-lists
  (assert-equal
    '("( 1 ( 2 ( 3 4 ) ) )")
    (tokenize "(1 (2 (3 4)))")))

;;; LITERALP tests
(define-test literalp-returns-t-for-int
  (assert-true
    (literalp "1")))

(define-test literalp-returns-t-for-float
  (assert-true
    (literalp "1.0")))

(define-test literalp-returns-t-for-float-w/o-leading-digit
  (assert-true
    (literalp ".1")))

(define-test literalp-returns-t-for-string
  (assert-true
    (literalp "\"a\"")))

(define-test literalp-returns-nil-for-variable
  (assert-false
    (literalp "a")))

(define-test literalp-returns-t-for-list
  (assert-true
    (literalp "()")))

;;; READ-FLUSH-LITERAL tests
(define-test read-flush-literal-correctly-reads-int
  (assert-equal
    1
    (read-flush-literal "1")))

(define-test read-flush-literal-correctly-reads-float
  (assert-equal
    1.0
    (read-flush-literal "1.0")))

(define-test read-flush-literal-correctly-reads-string
  (assert-equal
    "a"
    (read-flush-literal "\"a\"")))

(define-test read-flush-literal-correctly-reads-list
  (assert-equal
    '(1 2 3)
    (read-flush-literal "(1 2 3)")))

(define-test read-flush-literal-correctly-reads-nested-list
  (assert-equal
    '(1 (2 (3 4)))
    (read-flush-literal "(1 (2 (3 4)))")))

;;; RUN tests
(define-test run-pushes-int-onto-stack
  (assert-equal
    '(1)
    (run "1")))

(define-test run-pushes-string-onto-stack
  (assert-equal
    '("a")
    (run "\"a\"")))

(define-test run-pushes-list-onto-stack
  (assert-equal
    '((1 2 3))
    (run "(1 2 3)")))

(define-test run-pushes-multiple-items-onto-stack
  (assert-equal
    '("a" 1)
    (run "1\"a\"")))

(define-test run-calls-function-correctly
  (assert-equal
    '(3)
    (run "+ 1 2;")))

(define-test run-calls-function-correctly-with-multiple-args
  (assert-equal
    '(6)
    (run "+1 2 3;")))

(define-test run-little-flush-works-with-one-function-on-stack
  (assert-equal
    '(3)
    (run "+ 1 2,")))

(define-test run-little-flush-works-with-multiple-functions-on-stack
  (assert-equal
    '(5 1 +)
    (run "+ 1 + 2 3,")))

(define-test run-@-rotates-stack-correctly
  (assert-equal
    '(2 1 3)
    (run "1 2 3@")))
