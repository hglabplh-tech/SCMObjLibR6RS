(require "environment.scm" )
(require "internal-defs.scm" )
(display "address-tab tests")(newline)

(define addrtab (genaddr-tab)) 
(define first (make-variable 'first 'first #f #f mutable integer? #f))
(define second (make-variable 'second 'second #f #f immutable string? #f))
(define third (make-variable 'third 'third #f #f mutable list? #f))
(define ret (make-variable 'ret 'ret #f #f mutable integer? #f))

(define true-list? 
  (lambda(list) 
    (if (null? list) #t
        (if (not 
             (eq? #t (car list)))
            #f  
            (true-list? (cdr list))))))

(define check-contract
  (lambda (contr value)      
    (if (var-contract? contr)          
        (let* ((proc (var-contract-proc contr))                
               (c-parms (var-contract-params contr))
               (call-parms (append  (list value) c-parms)))            
          (printit "do real contract check with list")            
          (printit proc)
          (printit call-parms)
          (if (not (apply proc call-parms))
              #f
              #t))
        #t)))

(define make-method-call-new
  (lambda (ret-val-def parm-def addrtab)
    (lambda(proc . args)
      (let ((p-type-list (map variable-type? parm-def))
            (rettype? (variable-type? ret-val-def))
            (build-parmlist-env (make-parmlist-env addrtab))
            (ret-contract (variable-contract ret-val-def))
            (contract-list (map variable-contract parm-def)) ;; write additional procedure capable for map to check the contract
            (real-args (if (list? (car args))
                           (car args)
                           args)))          
        (let ((parms-ok-list 
               (map (lambda (p t) (p t)) p-type-list real-args))
              (contract-ok-list 
               (map check-contract contract-list real-args)))
          (if (and (true-list? parms-ok-list) (true-list? contract-ok-list))
              (let* ((parameters (map (lambda (p) (cons (variable-ident p) p)) parm-def))
                     (frame (build-parmlist-env parameters args)) 
                     (result  (apply proc frame)))
                (if (and (rettype? result) 
                         (check-contract ret-contract result))
                    result
                    (begin
                      (display rettype?) (display "wrong!!!!!")
                      'ret-type-error)))
              'parm-type-error))))))

(define test-call (make-method-call-new ret (list first second third) addrtab))
(test-call (lambda (one two three) 
             (display ((get-local addrtab) (cell-get-name two))) (newline)
             (display (cell-get two))(newline)  0)
           10 "Hello procedure" '(proc test rest))

((make-parmlist-env addrtab)(list (cons 'first first) (cons 'second second) (cons 'third third)) (list 5 "hello world" '(a b c)))


(define tester
  (lambda()
    (let ((first-value (addrtab-locateval addrtab (cdr (addrtab-find addrtab 'first))))
          (sec-value (addrtab-locateval addrtab (cdr (addrtab-find addrtab 'second))))                    
          (third-value (addrtab-locateval addrtab (cdr (addrtab-find addrtab 'third)))))      
      (display (cell-get-name first-value))(newline)
      (display (cell-get-class first-value))(newline)    
      (display (cell-get first-value))(newline)
      (display (cell-get-name sec-value))(newline)
      (display (cell-get-class sec-value))(newline)    
      (display (cell-get sec-value))(newline)
      (display (cell-get-name third-value))(newline)
      (display (cell-get-class third-value))(newline)    
      (display (cell-get third-value))(newline))))
(tester)    




(addrtab-empty? addrtab) ;;empty?


(addrtab-size addrtab) ;;size?


(addrtab-add  addrtab '(a b c d) '(a9 b8 c56 d88)) ;;add


(addrtab-empty?  addrtab) ;;empty?


(addrtab-size  addrtab) ;;size?





(addrtab-add  addrtab '(a z t k e) '(a6 z45 t10 k8 e7));;add


(display "locate")(newline)


(addrtab-locateval addrtab '(1 3))


(addrtab-locatesym addrtab '(1 3))


(addrtab-latest  addrtab);;latest


(addrtab-peek addrtab 0);;peek


(addrtab-peek addrtab 1);;peek


(addrtab-locateval addrtab (cdr (addrtab-find addrtab 'a)));;find

(display "find variables in address table") (newline)
(addrtab-find addrtab 'c);;find
(addrtab-locateval addrtab (cdr (addrtab-find addrtab 'c)))
(addrtab-add-val addrtab 'new "alpha")
(addrtab-latest  addrtab);;latest
(addrtab-find addrtab 'new);;find
(addrtab-locateval addrtab (cdr (addrtab-find addrtab 'new)))




(addrtab-find addrtab 't);;find


(addrtab-find addrtab 't);;find


(addrtab-find addrtab 'l);;find


(addrtab-size addrtab)


(addrtab-remove-latest addrtab) ;; remove


(addrtab-latest  addrtab);;latest


(addrtab-find addrtab 'c);;find


(addrtab-find addrtab 't);;find


(addrtab-size addrtab)


(define testit (make-vector 4 7))


(define testcopy (list testit))


(define restit (list->vector testcopy))


(vector-set! testit 0 10)


(vector-ref restit 0)


(car testcopy)



