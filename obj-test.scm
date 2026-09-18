#lang racket
(require "objectfactory.scm" )

;; here we place a get - method instead of the lambda and then we have a typed call 

(symbol? (class-def 'try))
(symbol? (class-def 'test 'try))



(method-def 'test 'print is-public (lambda(n t s) s) (list (cons 'one integer?) (cons 'two string?) (cons 'three list?)) vector?)

(printit (method-def 'test 'analyse is-public (lambda(n t m) (member-var-set! 'name "m-var-init")(display "member -> ")(display (member-var 'name)) (newline)(call-super-internal (list n t m)) (display m)(newline) (+ m m)) (list (cons 'test symbol?) (cons 'test2 string?) (vector (cons 'bla integer?) (list > 6))) (cons integer? (list > 12))) )

(printit (method-def 'test 'analyse is-public (lambda(n t) (member-var-set! 'base '(8 9 0))(display "member -> ")(display (member-var 'base))(newline) (call-super-internal (list n t)) (display n) (newline) 5) (list (cons 'label symbol?) (cons 'vname string?)) integer?) )

(printit (method-def 'try 'funny is-public (lambda(n t s) s)  (list (cons 'name symbol?)  (cons 'number integer?) (cons 'plop string?)) vector?) )

(printit (method-def 'try 'analyse is-public (lambda(n t) (display "super-method second analyse -- ") (display t) (newline) 9) (list (cons 'name symbol?)   (cons 'plop string?)) integer?) )

(printit (method-def 'try 'analyse is-public (lambda(n t m) (display "super-method call with contract-- ") (display t) (newline) m) (list (cons 'try symbol?) (cons 'tt string?) (vector (cons 'persnr integer?) (list check-memv 9 8 7 6))) integer?))

(printit (member-var-def 'test 'testcount integer? is-public))

(member-var-def 'test 'name string? is-private)

(member-var-def 'try 'base list? is-public)

(printit (member-var-def 'try 'group vector? is-public))

(define testobj (new-object 'test))
(printit testobj)
(send-message-internal testobj 'analyse 'test "bla" 7)  
(send-message-internal testobj 'analyse 'test "bla")  
(send-message-internal testobj 'analyse 'test "harry" 9)  
(send-message-internal testobj 'analyse 'test "heike")  
  ;(define genfunctab ())
  ;; only tests for address table
