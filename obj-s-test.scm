#lang mzscheme
(require (planet schematics/schemeunit:3:4)
         (planet schematics/schemeunit:3:4/text-ui)
         "objective-s.scm")
;;;
;;;
;HERE SOME TESTS

(define-sinterface  test-inter
  ((vorname string?) 
   (name string?))            
  ((get-this-name ((persnr integer?) (label symbol?)) string?))
  test-inter?)

(define-sinterface  test-inter-empl
  ((status integer?) 
   (abteilung string?))            
  ((get-this-name ((one integer?) (two symbol?)) string?))
  test-inter-empl?)


(define-sclass  person
  make-person-obj
  ((lambda       
       (in)
     (display "..init vorname is.. -> ")
     (display (member-var person.vorname))
     (member-var-set! person.vorname "Rudi")
     (member-var-set! person.name "Ratlos")
     (display " --- redefined --- ")
     (display in)
     (newline) #t))
  person?             
  ((name string? is-public "maier") 
   (vorname string? is-public "lulu" immutable))            
  ((get-this-name is-public (lambda (num sym)                         
                              (display num)(newline)
                              (display "passed symbol is: ")(display sym)(newline)
                              (display "actual name is: ")
                              (display (member-var person.vorname))
                              (display " - ")
                              (display (member-var person.name))(newline)
                              (member-var-set! person.vorname "gabi")
                              (display "vorname -- immutable !!!##****   ")(display (member-var person.vorname))(newline)
                              (member-var person.name))             
                  ((persnr integer? ) (label symbol?)) string? call-normal)
   (groovy-meth is-public (lambda (env) 
                            (display 'I-am-nearly-here)(newline)
                            (let ((lname (get-local-var env 'lastname)))
                              (display 'I-am-here)(newline)
                              lname))
                ((pnumber integer?) (firstname string?) (lastname string?) (address string?)) string? call-by-name)
   (get-vorname is-public (lambda(key) (string-append key "Berti"))
                ((key string?)) string? call-normal)))




(define-sclass  employee person 
  make-employee-obj            
  ((lambda                                
       (in)                                 
     (member-var-set! employee.status 5)                                 
     (member-var-set! employee.abteilung "none")                                 
     (display " --- redefined constructor subclass --- ")                                 
     (display in)                                 
     (newline) #t))
  employee?
  ((status integer? is-public 5 mutable) 
   (abteilung string? is-protected "Bekleidung" mutable < 9))             
  ;; look for inheritance rules and context change it loops round and round 
  ;; check if it functions better with old send-message-internal
  ((get-this-name is-public (lambda 
                                (num sym)               
                              (display "This is the class of: ")               
                              (display (member-var person.vorname))
                              (display " ")
                              (display (member-var person.name))
                              (member-var-set! person.name "test")
                              (newline)               
                              (display sym)               
                              (newline)               
                              (member-var-set! employee.abteilung "Käsetheke")               
                              (member-var-set! person.name "Glab")
                              (send-message (self$) employee.test-sc  "hallo hogo stinkst du so nach Käs und nach Bombo");; this is bullshit
                              (display (member-var employee.abteilung))               
                              (newline)
                              (call-super num sym))
                  (('persnr integer?  check-memv 9 6 7 90 100) ('label symbol?)) string? call-normal)
   (test-sc is-private (lambda 
                           (input) 
                         (display "I am private to empl")(newline)
                         (display "bull - shit")(newline)                         
                         (display input)(newline)
                         #t) ((test string? )) boolean? call-normal)  
   (meth-noparms is-public (lambda 
                               ()                         
                             "no-parm-method") () string? call-normal)))

(define-sclass  chief employee 
  make-chief-obj             
  ((lambda 
       (in)
     (member-var-set! employee.status 5)
     (member-var-set! employee.abteilung "none")
     (display " --- redefined constructor subclass --- ")                               
     (display in)                               
     (newline) #t))
  chief?
  ((status integer? is-public ) 
   (abteilung string? is-public))             
  ((get-this-name is-public (lambda 
                                (num sym)               
                              (display "This is the class of: ")               
                              (display (member-var person.vorname))
                              (newline)               
                              (display sym)               
                              (newline)               
                              (member-var-set! employee.abteilung "Käsetheke")               
                              (member-var-set! person.name "Glab")   
                              (send-message (self$) employee.test-sc  "hallo hugo bischt du Chef");; this is bullshit
                              (display (member-var employee.abteilung))               
                              (newline)
                              (call-super num sym))
                  (('persnr integer?) ('label symbol? )) string? call-normal)
   (test-sc is-private (lambda 
                           (input) 
                         (display "I am private to empl --> ")(display input)
                         (newline)
                         #t) ((bla string?)) boolean? call-normal)))

(define-sclass  list-class
  make-list-obj
  ((lambda       
       (in)     
     #t))
  list-class?             
  ((input-list list? is-public '(3 4 5 6 7 8 9 0) immutable))            
  ((add-to-list-copy is-public (lambda (numl) 
                                 (display numl)(newline)
                                 (if (eq? numl '()) '()
                                     (let* ((num (+ 9 (car numl) ))
                                            (temp-list (list num)))
                                       (append temp-list (send-message (self$) list-class.add-to-list-copy (cdr numl))))))
                     ((list-of-nums list? )) list? call-normal)))


(define oneperson (make-person-obj))
(display 'first)(newline)
(define test-employee (make-employee-obj))
(display 'second)(newline)
(define person-inst (make-checked-interface-instance oneperson test-inter))
(display 'third)(newline)
(define test-chief (make-chief-obj))
(display 'fourth)(newline)


(call-construct test-employee)
(call-construct test-chief)
;; test the west
(define simple-obj-tests
  (test-suite
   "Tests objective-s.scm"
   
   (check-false (is-obj-for-interface? test-inter test-employee) "check interface is true though it has to be false")
   (check-true  (is-obj-for-interface? test-inter oneperson) "check interface is false though it has to be true")
   (check-true  (is-obj-for-interface? test-inter-empl test-employee) "check interface is false though it has to be true")
   (check-true  (person? oneperson) "check of oneperson for type person #t --> ")   
   (check-true  (chief? test-chief) "check of test-chief for type chief #t --> ")
   (check-false  (person? test-chief) "check of test-chief for type person #f --> ")   
   (check-equal? (send-message person-inst person.get-this-name  9 'therapie) "Ratlos" "call to class-meth via interface went wrong")
   (check-equal? (send-message-by-names test-employee person.groovy-meth (lastname "Tusso") (pnumber 100) (firstname "Hanna"))  
                 "Tusso" "meth call via by-name")
   (check-equal? (send-message test-employee person.get-this-name  90 'bla) "Glab" "we wait for Glab as result")
   (check-equal? (send-message test-chief  person.get-this-name  9 'bla) "Glab" "wait Glab as result")
   (check-equal? (member-var person.vorname test-chief) "lulu" "we wait for lulu")
   (check-equal? (send-message test-chief  person.get-vorname "Das ist der ") "Das ist der Berti" "We wait for: Das ist der Berti")
   (check-equal? (send-message test-employee employee.test-sc "hallo hogo stinkst du so nach Käs und nach Bombo") #f "wait for #f for mth is private")
   (check-equal? (send-message test-employee employee.meth-noparms) "no-parm-method" "no parms call failed")
   (test-case
    "List has length 4 and all elements even"
    
    (let ((theObj (make-list-obj)))
      (check-equal? (send-message theObj list-class.add-to-list-copy (member-var list-class.input-list theObj)) '(12 13 14 15 16 17 18 9))))))


(run-tests simple-obj-tests)


