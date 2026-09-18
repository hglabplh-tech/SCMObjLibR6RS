;; Copyright (c) 2007-2010 by Harald Glab-Plhak.
;; objective extension for mzscheme and 
;; later on scheme48 author : Harald Glab-Plhak
(module objectfactory mzscheme
  (provide class-def method-def 
           member-var-def send-message-internal
           send-message-internal-call-by-name
           make-method-call
           call-super-internal 
           member-var 
           member-var-set!
           new-object
           redefine-constructor
           is-public is-protected
           is-private  
           mutable immutable       
           call-construct
           self$
           isofclass
           interf-def
           is-obj-for-interface?
           check-memq check-memv
           make-checked-interface-instance
           call-by-name
           call-normal
           get-local-var
           printit)
  
  (require "internal-defs.scm")
  (require "obj-interfaces.scm" )
  (require "o-exceptions.scm" )
  (require "environment.scm" )
  (require "types.scm" )  
  (require (lib "exception.ss" "srfi" "34"))  
  (require (lib "etc.ss"))
  
  (define call-by-name 'by-name)
  (define call-normal 'normal)
  (define instanceof
    (lambda (thing thing-of)
      (cond ((class-anchor? thing)
             (cond ((selfp? thing-of)
                    #t)
                   ((interface-inst? thing-of)
                    #t)))
            ((interface-def? thing)
             (cond ((selfp? thing-of)
                    #t)
                   ((interface-inst? thing-of)
                    #t))))))
  
  
  
  (define isofclass
    (lambda (anchor)    
      (lambda (obj)      
        (let ((obj-anchor (selfp-obj-c-anchor obj)))        
          (obj-equal? anchor obj-anchor)))))
  
  (define find-all-methods
    (lambda (ident list-of-pairs)
      (if (null? list-of-pairs) 
          '()    
          (let ((act-pair (car list-of-pairs)))      
            (cond ((eq? (car act-pair) ident)             
                   (cons (cons (method-short-sig (cdr act-pair)) (cdr act-pair)) (find-all-methods ident (cdr list-of-pairs))))
                  (else (find-all-methods ident (cdr list-of-pairs))))))))
  
  (define check-method-incl-sig  
    (lambda (act-method vtab)
      (printit "method identification done inclusive signature")
      (let ((meth-list (find-all-methods (method-name act-method) vtab))
            (meth-short-sig (method-short-sig act-method)))
        (if (not (null? meth-list))
            (let ((method-candidate (assoc  meth-short-sig meth-list)))            
              (if method-candidate              
                  (cdr method-candidate)                
                  #f))
            #f))))
  
  (stackdef ctx-stack (ctx-stack-push! ctx-stack-pop ctx-stack-empty? ctx-stack-init ctx-stack-top))
  
  (define make-actual-context  
    (let ((nesting 0))
      (lambda ()
        (letrec ((push-context! (lambda (self env)
                                  (let ((cont (make-context self env)))
                                    (ctx-stack-push! cont)                                   
                                    cont)))                  
                 (get-context (lambda () (if (not (ctx-stack-empty?))
                                             (ctx-stack-top)
                                             (make-context #f #f))))
                 (pop-context (lambda () (ctx-stack-pop) ))                 
                 (increment (lambda () (set! nesting (+ nesting 1))))
                 (decrement (lambda () (set! nesting (- nesting 1))))
                 (get-nesting (lambda () nesting))) 
          (vector push-context! get-context pop-context increment decrement get-nesting)))))
  
  (define actual-context (make-actual-context))  
  
  (define class-type-registry (make-type-registry))
  
  (define class-def
    (opt-lambda (class-name 
                 (extends '())
                 (interfaces '()))
      (let* ((class-extend
              (cond ((null? extends) extends)
                    (else (generate-sym extends))))
             (class-interface 
              (cond ((null? interfaces) interfaces)
                    (else (map generate-sym interfaces))))
             (super-class (reg-get-class-type class-type-registry class-extend))
             (super-list (if (and (not (null? super-class)) super-class)                      
                             (cons class-extend (class-anchor-super-list (class-getter (car super-class))))
                             (append '() class-extend)))  
             (class-anchor-def 
              (make-class-anchor class-name (generate-sym class-name) class-extend super-list class-interface #f '() '() '() '() #f )))
        (printit "super-list")
        (printit super-list)
        (reg-add-class-type class-type-registry class-anchor-def)
        (set-class-anchor-constructor! class-anchor-def (method-def class-name class-name is-public                    
                                                                    (lambda                        
                                                                        (name)                      
                                                                      #t)                    
                                                                    (list (cons 'c-name string?))
                                                                    boolean?))
        class-anchor-def)))
  
  
  (define produce-signaturepart-arg 
    (lambda(m-name parms call-op)   
      (let ((signature (if (eq? call-op call-by-name)
                           (generate-sym           
                            (string-append                       
                             (symbol->string m-name)            
                             "_by-name"))
                           (generate-sym         
                            (string-append                          
                             (symbol->string m-name)            
                             "_"                             
                             (get-symbolpart-forarguments parms))))))
        signature)))
  
  (define produce-signaturepart-parm 
    (lambda(m-name parm-list call-op) 
      (let ((signature (if (eq? call-op call-by-name)
                           (generate-sym           
                            (string-append                       
                             (symbol->string m-name)            
                             "_by-name"))            
                           (generate-sym           
                            (string-append                       
                             (symbol->string m-name)            
                             "_"            
                             (get-symbolpart-forparms (map variable-type? parm-list)))))))
        signature)))
  
  ; generate a method symbol as a mangling 
  ;name for the method identifying the method as unique type name
  ; including the class-name the method name and the method parameter type definition
  ; put this part of the stuff below in a separate define 
  
  
  (define method-def
    (opt-lambda (class-name m-name m-class m-proc m-parmtype-list m-ret (call-op call-normal))
      (let* ((parm-list (build-parmlist  m-parmtype-list))
             (ret-var (build-return m-ret))
             (meth-short-sig (produce-signaturepart-parm m-name parm-list call-op))
             (method-sig        
              (generate-sym           
               (string-append              
                (symbol->string class-name)
                "#"
                (symbol->string meth-short-sig))))             
             (method-struct (make-method m-name m-class meth-short-sig method-sig #f 
                                         ret-var parm-list m-proc  (if (eq? call-op call-by-name)                                         
                                                                       (let ((p-list (build-parmlist  (list (cons 'env vector?)))))
                                                                         p-list)
                                                                       call-op) #f))
             (class-anchor-def (cdr (reg-get-class-type class-type-registry class-name)))           
             (c-anchor (cadr (cdr class-anchor-def)))             
             (c-methods (cond ((eq? m-class is-public)
                               (class-anchor-pub-methods c-anchor))                          
                              ((eq? m-class is-protected)
                               (class-anchor-prot-methods c-anchor))                          
                              ((eq? m-class is-private)
                               (class-anchor-priv-methods c-anchor))))
             (meth-definition (assoc method-sig c-methods)))
        (printit'begin----tttt-parm-list)
        (printit meth-short-sig)
        (printit parm-list)
        (printit'end----tttt-parm-list)
        (if (not meth-definition) 
            (cond ((eq? m-class is-public)
                   (set-class-anchor-pub-methods! c-anchor                                         
                                                  (cons (cons method-sig method-struct)                                               
                                                        c-methods)))
                  ((eq? m-class is-protected)                                       
                   (set-class-anchor-prot-methods! c-anchor                                                                  
                                                   (cons (cons method-sig method-struct)                             
                                                         c-methods)))
                  ((eq? m-class is-private)                                        
                   (set-class-anchor-priv-methods! c-anchor                                         
                                                   (cons (cons method-sig method-struct)                                               
                                                         c-methods))))              
            (set-method-proc! (cdr meth-definition) m-proc))
        (printit "method after declaration")
        (printit method-struct)
        method-struct)))
  
  (define find-method-in-tree
    (lambda (c-anchor meth-short-sig scope-list)
      (let ((meth-sig (generate-sym           
                       (string-append              
                        (symbol->string (class-anchor-name c-anchor))
                        "#"
                        (symbol->string meth-short-sig)))))
        (let* ((meths (append (if (memv is-public scope-list)
                                  (class-anchor-pub-methods c-anchor)
                                  '())
                              (if (memv is-protected scope-list)                           
                                  (class-anchor-prot-methods c-anchor)
                                  '())
                              (if (memv is-private scope-list)                           
                                  (class-anchor-priv-methods c-anchor)
                                  '())))               
               (real-method-def (assoc meth-sig meths)))
          (if real-method-def
              real-method-def
              (let ((s-class-name (class-anchor-super c-anchor)))
                (if s-class-name               
                    (let ((s-c-anchor (class-getter s-class-name)))
                      (if (class-anchor? s-c-anchor)
                          (find-method-in-tree s-c-anchor meth-short-sig scope-list)
                          #f))
                    #f)))))))
  
  
  
  (define redefine-constructor
    (lambda (c-anchor proc)
      (let ((name (class-anchor-name c-anchor)))
        (method-def name name is-public proc (list (cons 'c-name string?)) boolean?))))
  
  
  (define member-var-def
    (opt-lambda (class-name var-name type v-class 
                            (value '??value??) (stor-class mutable)  (contract #f) )            
      (let ((contr (if (pair? contract)
                       (make-var-contract (car contract) (cdr contract))
                       #f)))             
        (let* ((variable-struct (make-variable var-name var-name value #t stor-class type contr v-class))
               (class-anchor-def (cdr (reg-get-class-type class-type-registry class-name)))         
               (var-sym          
                (generate-sym           
                 (string-append            
                  (symbol->string class-name)            
                  "#" (symbol->string var-name))))              
               (c-anchor (cadr (cdr class-anchor-def))))                        
          (set-variable-ident! variable-struct var-sym)
          (set-class-anchor-variables! c-anchor      
                                       (cons (cons var-name variable-struct) 
                                             (class-anchor-variables c-anchor)))
          
          variable-struct))))
  
  (define true-list? 
    (lambda(list) 
      (if (null? list) #t
          (if (not 
               (eq? #t (car list)))
              #f  
              (true-list? (cdr list))))))
  
  
  
  (define generate-object-ident
    (let ((counter 0))
      (lambda (class-name)
        (let* ((act-counter (+ counter 1))
               (c-name-string (if (symbol? class-name)
                                  (symbol->string class-name)
                                  class-name))
               (counter-string (number->string act-counter)))
          (set! counter act-counter)
          (generate-sym (string-append c-name-string counter-string))))))        
  
  
  
  
  
  (define class-getter 
    (lambda (c-sym) 
      (let ((result (reg-get-class-type class-type-registry c-sym)))
        (if result      
            (cadr (cddr result))
            #f))))
  
  
  (define object-pool (make-object-pool))
  (define add-object (pool-add-object  object-pool))    
  (define get-object (pool-get-object  object-pool))
  
  (define c-new 
    (lambda (c-anchor)
      (let* ((super-class-id (class-anchor-super c-anchor))
             (super-class (reg-get-class-type class-type-registry super-class-id))
             (s-obj (if (and (not (null? super-class)) super-class)                      
                        (c-new (class-getter (car super-class)))
                        #f)))        
        (let* ((member-list (class-anchor-variables c-anchor))               
               (member-cell-list (make-membervar-cells member-list))
               (dummy (make-setter/getter c-anchor member-cell-list))
               (completed-member-cell-list (if super-class ;; Mike Sperber said that's enough                  
                                               (append member-cell-list (selfp-obj-env s-obj))                  
                                               member-cell-list))       
               (meth-list-pub (make-normalized-method-list (class-anchor-pub-methods c-anchor)))
               (meth-list-prot (make-normalized-method-list (class-anchor-prot-methods c-anchor)))
               (meth-list-priv (make-normalized-method-list (class-anchor-priv-methods c-anchor)))
               (completed-meth-list-pub (if super-class
                                            (append meth-list-pub (selfp-obj-vtab-pub s-obj))
                                            meth-list-pub))
               (completed-meth-list-prot (if super-class
                                             (append meth-list-prot (selfp-obj-vtab-prot s-obj))
                                             meth-list-prot))
               (object (make-selfp (generate-object-ident (class-anchor-qualified-name c-anchor))
                                   (class-anchor-qualified-name c-anchor) 
                                   c-anchor
                                   completed-member-cell-list
                                   completed-meth-list-pub 
                                   completed-meth-list-prot
                                   meth-list-priv
                                   s-obj #f)))
          (add-object 'global (selfp-obj-ident object) object)
          (call-construct-internal object)
          object))))
  
  (define make-setter/getter 
    (lambda (c-anchor member-cell-list)
      (if (eq? member-cell-list '())
          '()
          (let* ((cell (cdr (car member-cell-list)))
                 (class-name (class-anchor-name c-anchor))
                 (var-name (cell-get-name cell))
                 (v-class (cell-get-v-class cell))
                 (type (cell-get-type? cell)))  
            (printit "theType")(printit type)(newline)
            (method-def class-name (generate-sym (string-append "get-" (symbol->string (cell-get-name cell)))) 
                        v-class (lambda () (cell-get cell)) '() type)
            (method-def class-name (generate-sym (string-append "set-" (symbol->string (cell-get-name cell)))) 
                        v-class (lambda (val) (cell-set! cell val)) (list (cons 'setval type)) boolean?)
            (make-setter/getter c-anchor (cdr member-cell-list))))))
  
  
  (define make-method-call
    (lambda (ret-val-def parm-def)
      (lambda(proc . args)
        (printit ret-val-def)
        (printit parm-def)
        (let ((p-type-list (map variable-type? parm-def))
              (p-name-list (map variable-name parm-def))
              (rettype? (variable-type? ret-val-def))
              (ret-contract (variable-contract ret-val-def))
              (contract-list (map variable-contract parm-def)) ;; write additional procedure capable for map to check the contract
              (real-args (if (list? (car args))
                             (car args)
                             args))) 
          (printit p-type-list)
          (printit real-args)
          (let ((parms-ok-list 
                 (map (lambda (p t) (p t)) p-type-list real-args))
                (contract-ok-list 
                 (map check-contract contract-list real-args)))
            (if (and (true-list? parms-ok-list) (true-list? contract-ok-list))
                (let ((result 
                       (if (and (list? real-args) (eq? (length args) 1))
                           (apply proc real-args)
                           (proc real-args))))
                  (if (and (rettype? result) 
                           (check-contract ret-contract result))
                      
                      result
                      (begin
                        (display rettype?) (display "wrong!!!!!")(display p-name-list) ;; here is a place for a well define exception
                        'ret-type-error)))
                'parm-type-error))))))
  
  (define make-method-call-by-name
    (lambda (ret-val-def real-method)
      (lambda(proc env)
        (printit "real method is ->")
       
        (printit proc)
        (let* ((parm-def (method-opts real-method))
               (p-type-list (map variable-type? parm-def))
               (p-name-list (map variable-name parm-def))
               (rettype? (variable-type? ret-val-def))
               (ret-contract (variable-contract ret-val-def))
               (contract-list (map variable-contract parm-def))) ;; write additional procedure capable for map to check the contract
          (let ((call-ok? (vector? env))) ;; this sep is done foorgeting later on a contract for this stuff too                
            (if call-ok?
                (let ((result (proc env)))
                  (if (and (rettype? result) 
                           (check-contract ret-contract result));; the return contrct is already active                      
                      result
                      (begin
                        (display rettype?) (display "wrong!!!!!")(display p-name-list) ;; here is a place for a well define exception
                        'ret-type-error)))
                'parm-type-error))))))
  
  (define find-spec-parm-def     
    (lambda (parm-list) 
        (lambda (name)
          (letrec  ((loop (lambda (p-list)
          (if (eq? p-list '()) 
              'error
              (if (eq? (variable-name (car p-list)) name)
                  (car p-list)
                  (loop  (cdr p-list)))))))                    
            (loop parm-list)))))

  (define make-env-callby-name
    (lambda (method-def arg-list)
      (printit (method-parms method-def))
      (printit "ready")
      (let* ((ret-type (method-type method-def))                                 
             (parm-def (method-parms method-def))
             (find-parm-proc (find-spec-parm-def parm-def))
             (meth-proc  (method-proc method-def))
             (arg-names  (map car arg-list))
             (arg-values (map cdr arg-list))
             (arg-p-list (map find-parm-proc arg-names)))        
        (let* ((local-env (genaddr-tab))
               (the-fun-environment ((make-parmlist-env local-env) 
                                      arg-p-list arg-values)))
          (vector (get-local local-env))))))
  
  (define get-local-var 
    (lambda (env name) 
      ((vector-ref env 0) name)))
  
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
  
  (define check-memq 
    (lambda (value . more)
      (let ((parm2 more))       
        (if (not (memq value parm2))
            #f
            #t))))
  
  (define check-memv 
    (lambda (value . more)
      (let ((parm2 more))        
        (let ((result (if (not (memv value parm2))
                          #f
                          #t)))          
          result))))

  (define really-call-method 
    (opt-lambda (selfp-ref meth-ref m-call meth-proc (params '()))
      (call-with-current-continuation
       (lambda (cont)       
         (with-exception-handler
          (lambda (parm)            
            (if (o-error? parm)                  
                (error (o-error-msg parm))))                      
                ;;(error parm)))
          (lambda ()
            (dynamic-wind 
             (lambda()                      
               (let ((this-pointer (make-this-ref selfp-ref meth-ref #f #f #f #f )))                        
                 ((vector-ref actual-context 3))                        
                 (printit ((vector-ref actual-context 5)))                        
                 ((vector-ref actual-context 0) this-pointer #f)                        
                 this-pointer))                    
             (lambda ()                      
               (call-with-current-continuation                       
                (lambda (k)                         
                  (with-exception-handler
                   (lambda (parm)
                     (raise parm))
                   ;(cont 'zero))                       
                   (lambda ()   
                     (printit "immediate before the method is called after build env")
                     (let ((result (m-call meth-proc params)))
                       (set-selfp-result! selfp-ref result)
                       result)))))) 
             (lambda ()                      
               ((vector-ref actual-context 4))                      
               (printit ((vector-ref actual-context 5)))                      
               ((vector-ref actual-context 2))))))))))
  
  
  
  
  (define get-self-context
    (lambda ()
      (let ((this-reference (context-this ((vector-ref actual-context 1)))))
        (if this-reference
            (let ((object (this-ref-selfp this-reference)))
              object)
            #f))))
  
  (define self$
    (lambda ()
      (let ((result (get-self-context)))
        (if (not result)
            #f ;raise exception
            result))))
  
  
  
  ;; this definition is not really ready
  ;; it has to be checked if the definition is ok for each case  
  
  (define check-against-context-obj
    (lambda (cont-obj object)
      (if (not (selfp? cont-obj))
          (list is-public)
          (let ((cont-c-anchor (selfp-obj-c-anchor cont-obj))
                (c-anchor (selfp-obj-c-anchor object)))
            (cond ((eq? (class-anchor-name cont-c-anchor)
                        (class-anchor-name c-anchor))                
                   (list is-public is-protected is-private))                 
                  ((memv (class-anchor-name c-anchor)
                         (class-anchor-super-list cont-c-anchor))
                   (list is-public is-protected))
                  (else (list is-public)))))))
  
  ;; definition to send a message to any existing object
  ;; a message is nearly the same as a procedure call 
  ;; in this case only procedures residing 
  ;; in the given object can be called
  
  
  (define send-message-internal
    (lambda (thing message . parms)
      (cond ((selfp? thing)
             (send-message-internal-object thing message call-normal parms))
            ((interface-inst? thing)
             (send-message-internal-interface thing message call-normal parms))
            (else #f))))
  
  (define send-message-internal-call-by-name
    (lambda (thing message . parms)
      (cond ((selfp? thing)
             (send-message-internal-object thing message call-by-name parms))
            ((interface-inst? thing)
             (error 'not-supported-yet))
            (else #f))))
  
  
  (define send-message-internal-interface
    (opt-lambda (interf-inst message (call-op call-normal) (parms '()))
      (let ((obj-ref (interface-inst-selfp interf-inst)))
        (let*((obj-pair (get-object 'global (selfp-obj-ident obj-ref)))
              (meth-short-sig (generate-sym           
                               (string-append                          
                                (symbol->string message)            
                                "_"                             
                                (get-symbolpart-forarguments parms))))              
              (check-dummy-method (make-method  message is-public meth-short-sig #f #f #f #f #f #f #f)))          
          (printit "-------------------------- entered call method by interface -------------------------")
          (if (pair? obj-pair)
              (let* ((object (cdr obj-pair))
                     (methods-list-obj (selfp-obj-vtab-pub object))
                     (methods-list-interf (interface-def-methods 
                                           (interface-inst-def interf-inst)))
                     (meth-in-obj (assoc message methods-list-obj))
                     (meth-in-iface-inst (assoc message methods-list-interf)))
                (if (and (pair? meth-in-obj) (pair? meth-in-iface-inst))
                    (let ((real-method (cdr meth-in-obj)))
                      (really-call-meth-proc real-method object call-op parms)))))))))
  
  (define send-message-internal-object
    (opt-lambda (obj-ref message (call-op call-normal) (parms '()))
      (let*((obj-pair (get-object 'global (selfp-obj-ident obj-ref)))
            (my-obj (get-self-context))
            (nest-counter ((vector-ref actual-context 5)))
            (meth-short-sig (produce-signaturepart-arg message parms call-op))            
            (check-dummy-method (make-method  message is-public meth-short-sig #f #f #f #f #f #f #f)))
        (printit "method definition of:: real meth signature --> ")
        (printit meth-short-sig)
        (if (pair? obj-pair)
            (let ((object (cdr obj-pair)))            
              (if (selfp? object)
                  (let* ((obj-c-anchor (selfp-obj-c-anchor object))
                         (c-name (selfp-obj-c-type object))                       
                         (comp-c-name (cond ((selfp? my-obj)
                                             (selfp-obj-c-type my-obj))
                                            (else '&not-eval)))
                         (real-method-def (find-method-in-tree 
                                           obj-c-anchor meth-short-sig                                                              
                                           (check-against-context-obj my-obj object))))
                    (printit "method definition of:: real meth --> ")
                    (printit real-method-def)
                    (if (or (not (eq? nest-counter 0)) (and (eq? nest-counter 0) real-method-def))
                        (let* ((methods-list (append (selfp-obj-vtab-pub object) (selfp-obj-vtab-prot object) 
                                                     (if (and (not (eq? nest-counter 0)) (eq? c-name comp-c-name))
                                                         (selfp-obj-vtab-priv object)
                                                         '())))
                               (real-method (check-method-incl-sig check-dummy-method  methods-list)))
                          (printit "real-method")
                          (printit real-method)
                          (really-call-meth-proc real-method object call-op parms))
                        #f))
                  #f))))))
  
  (define really-call-meth-proc 
    (lambda (real-method object call-op parms)
      (printit "aguments ----> ")
      (printit parms)    
      (printit "<------- arguments")
      (if real-method
          (if (equal? call-op call-normal)
              (let* ((ret-type (method-type real-method))                                 
                     (parm-def (method-parms real-method))                                     
                     (meth-proc  (method-proc real-method))
                     (params parms)
                     (m-call (make-method-call ret-type parm-def))                                                            
                     (res (really-call-method object real-method m-call meth-proc params)))                
                res)
              (let* ((ret-type (method-type real-method))                                 
                     (parm-def (method-parms real-method))                                     
                     (meth-proc  (method-proc real-method))
                     (env (make-env-callby-name real-method parms)))
                (let* ((class-name (class-anchor-name (selfp-obj-c-anchor object)))
                       (m-call (make-method-call-by-name ret-type real-method))                   
                       (res (really-call-method object real-method m-call meth-proc env)))
                  res)))
          #f)))
  
  (define call-construct
    (lambda (obj-ref)
      (call-construct-internal obj-ref)))
  
  
  (define call-construct-internal
    (lambda (obj-ref)  
      (printit "call-construct")
      (let ((obj-pair (get-object ;; check if the object is really from this environment
                       'global                       
                       (selfp-obj-ident obj-ref)))) 
        (printit (selfp-obj-ident obj-ref))
        (if obj-pair            
            (let* ((real-obj (cdr obj-pair))                   
                   (anchor (selfp-obj-c-anchor real-obj))
                   (class-name-str (if (symbol? (class-anchor-name anchor))
                                       (symbol->string (class-anchor-name anchor))
                                       (class-anchor-name anchor)))                    
                   (real-method (class-anchor-constructor anchor)))
              (printit real-method)
              (if real-method                          
                  (let* ((ret-type (method-type real-method))                                 
                         (parm-def (method-parms real-method))                                                          
                         (meth-proc  (method-proc real-method))                                 
                         (m-call (make-method-call ret-type parm-def))                                 
                         (params (list class-name-str)))  
                    (printit "before really call method")
                    (really-call-method real-obj real-method m-call meth-proc params)) 
                  #f))))))

  (define call-super-internal 
    (lambda (params)
      (let* ((this-reference (context-this ((vector-ref actual-context 1))))
             (this-method (this-ref-act-method this-reference)))
        (printit this-reference)
        (let* ((object (this-ref-selfp this-reference))               
               (super-obj (selfp-obj-super-obj object))           
               (s-vtab (append (selfp-obj-vtab-pub super-obj) (selfp-obj-vtab-prot super-obj)))             
               (real-method (check-method-incl-sig this-method s-vtab)))
          (if (selfp? super-obj)
              (begin
                (printit "actual object -->")
                (printit (selfp-obj-ident object))
                (printit "actual super object -->")
                (printit (selfp-obj-ident super-obj))
                (if (and real-method super-obj)
                    (let* ((ret-type (method-type real-method))
                           (parm-def (method-parms real-method))
                           (meth-proc  (method-proc real-method))                      
                           (m-call (make-method-call ret-type parm-def)))
                      (printit "method during call setup")
                      (printit real-method)
                      (really-call-method super-obj real-method m-call meth-proc params))            
                    #f))
              #f)))))
  
  
  (define member-var-set!
    (opt-lambda (var-name value (obj (self$)))
      (let ((meth-sym (generate-sym (string-append "set-" (symbol->string var-name)))))
        (send-message-internal obj meth-sym value))))
  
  (define member-var
    (opt-lambda (var-name (obj (self$)))
      (let ((meth-sym (generate-sym (string-append "get-" (symbol->string var-name)))))
        (send-message-internal obj meth-sym))))
  
  
  
  (define new-object 
    (lambda (c-name)
      (c-new (class-getter c-name))))
  
  )
