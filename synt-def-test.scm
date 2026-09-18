(define-syntax (define-sclass sclass-stx)
    (syntax-case sclass-stx ()       
      ((_ class-name make-obj predicate?
          ((var-name var-pred?) ...)
          ((meth-name proc (type-list ...) ret-pred?) ...))
       (with-syntax ()          
         (syntax
          (begin
            (define class-name (class-def 'class-name))             
            (define make-obj (lambda () (new-object 'class-name)))
            (define predicate? 'obj)
            (apply member-var-def (list 'class-name 'var-name var-pred?)) ...
            (apply method-def  (list 'class-name 'meth-name proc (type-list ...) ret-pred?)) ...))))
      ((_ class-name super-class make-obj predicate? 
          ((var-name var-pred?) ...)
          ((meth-name proc (type-list ...) ret-pred?) ...))
       (with-syntax ()           
         (syntax                 
          (begin
            (define class-name (class-def 'class-name 'super-class)) 
            (define s-class 'super-class)
            (define make-obj (lambda () (new-object 'class-name)))
            (define predicate? 'obj)
            (apply member-var-def (list 'class-name 'var-name var-pred?)) ...
            (apply method-def  (list 'class-name 'meth-name proc (type-list ...) ret-pred?)) ...))))))
          


(define-syntax (define-methods smeth-stx)
    (syntax-case smeth-stx ()       
      ((_  class-name (meth-name proc (type-list ...) ret-pred?) ...)
       (with-syntax
           (
            )
         (syntax
          (begin
            (method-def 'class-name 'meth-name  proc (list (type-list ...)) ret-pred?) ...)
           )))))

(define-syntax (define-members smember-stx)
    (syntax-case smember-stx ()       
      ((_  class-name (var-name var-pred?)  ...)
       (with-syntax
           (
            )
         (syntax
          (begin
            (member-var-def 'class-name 'var-name var-pred?) ...)
           )))))
      

(require (lib "records.ss" "srfi" "57"))
(require (lib "exception.ss" "srfi" "34"))
(require (lib "condition.ss" "srfi" "35"))

(define-record-type class-anchor
  (make-class-anchor name qualified-name super interfaces 
                     constructor variables methods finalizer)
  class-anchor?  
  (name class-anchor-name set-class-anchor-name!)
  (qualified-name class-anchor-qualified-name 
                  set-class-anchor-qualified-name!)
  (super class-anchor-super set-class-anchor-super!)
  (interfaces class-anchor-interfaces set-class-anchor-interfaces!)
  (constructor class-anchor-constructor set-class-anchor-constructor!)
  (variables class-anchor-variables set-class-anchor-variables!)
  (methods class-anchor-methods set-class-anchor-methods!)
  (finalizer class-anchor-finalizer set-class-anchor-finalizer!))

(define-record-type variable 
  (make-variable ident name member? type?)  
  variable?
  (ident variable-ident set-variable-ident!)
  (name variable-name)
  (member? variable-member? set-variable-member?)  
  (type? variable-type?))

(define-record-type method 
  (make-method name short-sig signature super type parms proc)
  method?  
  (name method-name set-method-name!)
  (short-sig method-short-sig set-method-short-sig!)
  (signature method-signature set-method-signature!)
  (super method-super set-method-super!)
  (type method-type set-method-type!);return - type
  (parms method-parms set-method-parms!)
  (proc method-proc set-method-proc!))

(define-record-type selfp
  (make-selfp obj-ident obj-c-type obj-c-anchor obj-env obj-vtab obj-super-class)
  selfp?
  (obj-ident selfp-obj-ident set-selfp-obj-ident!)
  (obj-c-type selfp-obj-c-type set-selfp-obj-c-type!)
  (obj-c-anchor selfp-obj-c-anchor set-selfp-obj-c-anchor!)
  (obj-env selfp-obj-env set-selfp-obj-env!)
  (obj-vtab selfp-obj-vtab set-selfp-obj-vtab!)  
  (obj-super-class selfp-obj-super-class set-obj-selfp-super-class!))

(define-record-type this-ref
  (make-this-ref selfp act-method call-super var-get var-set! var-member?)
  this-ref?
  (selfp this-ref-selfp) 
  (act-method this-ref-act-method)
  (call-super this-ref-call-super)
  (var-get this-ref-var-get)
  (var-set! this-ref-var-set!)             
  (var-member? this-ref-var-member?))

(define-record-type context
  (make-context this env)
  context?
  (this context-this)
  (env context-env))
  


;;record helper

(define print-method 
  (lambda (meth)
    (if (method? meth)        
        (begin 
          (printit "--- print out of method ---")
          (printit (method-name meth))
          (printit (method-short-sig meth))
          (printit (method-signature meth))
          (printit (method-super meth))
          (printit (method-type meth))
          (printit (method-parms meth))
          (printit (method-proc meth))
          (printit "--- end print out of method ---")))))

(define print-variable 
  (lambda (var)
    (if (variable? var)        
        (begin 
          (printit "--- print out of variable ---")
          (printit (variable-name var))
          (printit (variable-member? var))
          (printit (variable-type? var))
          (printit "--- end print out of variable ---")))))

(define print-selfp
  (lambda (selfp)
    (if (selfp? selfp)        
        (begin 
          (printit "--- print out of selfp ---")
          (printit (selfp-obj-ident selfp))
          (printit (selfp-obj-c-type selfp))
          (printit (selfp-obj-c-anchor selfp))
          (printit (selfp-obj-env selfp))
          (printit (selfp-obj-vtab selfp))
          (printit (selfp-obj-super-class selfp))
          (printit "--- end print out of selfp ---")))))

(define print-this-ref
  (lambda (this-ref)
    (if (this-ref? this-ref)        
        (begin 
          (printit "--- print out of this reference ---")
          (printit (this-ref-selfp this-ref))
          (printit "--- end print out of this reference ---")))))

(define print-context
  (lambda (cont)
    (if (context? cont)        
        (begin 
          (printit "--- print out of context ---")
          (printit (context-this cont))
          (printit (context-env cont))
          (printit "--- end print out of context ---")))))
               
               
                        
(define print-class-anchor
  (lambda (ca)
    (if (class-anchor? ca)
  
        (begin 
          (printit "--- class - anchor - record - print ---")          
          (printit (class-anchor-name ca))          
          (printit (class-anchor-qualified-name ca)) 
          (printit (class-anchor-super ca)) 
          (printit (class-anchor-interfaces ca)) 
          (printit (class-anchor-constructor ca)) 
          (printit (class-anchor-variables ca))
          (printit (class-anchor-methods ca))
          (printit (class-anchor-finalizer ca))
          (printit "--- end class - anchor - record - print ---") ))))

(define really-print #t)

(define printit 
  (lambda (in)
    (cond ((method? in)
           (print-method in))
          ((variable? in)
           (print-variable in))      
          ((class-anchor? in)       
           (print-class-anchor in))      
          ((selfp? in)       
           (print-selfp in)) 
          ((this-ref? in)
           (print-this-ref in))
          ((context? in)
           (print-context in))
          (else 
           (if really-print
               (begin
                 (display in)
                 (newline)))))))
  
(define make-simple-object 
  (lambda()
    (letrec 
        ((new (lambda()#f))
         (init (lambda()#f))
         (cleanup (lambda()#f)))
      (vector new init cleanup))))


(define make-obj-vtab 
  (lambda ()
    (let ((inherit-list '()))
      (letrec 
          ((new-level (lambda() (#f)))
           (remove-level (lambda ()(#f)))
           (get-level (lambda ()(#f)))
           (get-level-super (lambda ()(#f))))
        (vector new-level 
                remove-level 
                get-level 
                get-level-super)))))


; some utilities
(define obj-hash-counter
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      counter)))
       
(define generate-sym 
  (lambda (value)
    (if (null? value) 
        value    
        (if (not (symbol? value))        
            (string->symbol value)        
            value))))

(define base-types
    (list (cons vector? 'vector?)           
          (cons list? 'list?)           
          (cons string? 'string?)           
          (cons integer? 'integer?)          
          (cons real? 'real?)          
          (cons complex? 'complex?)          
          (cons number? 'number?)          
          (cons procedure? 'procedure?)          
          (cons symbol? 'symbol?)          
          (cons char? 'char?)          
          (cons pair? 'pair?)))

;use this bit to get the data type of a parameter and to distinct aftwerwords the method signature
; update base-types for this reason
(define get-param-type
  (lambda (in)
    (cond ((vector? in)
           (list-ref base-types 0))
          ((list? in)
          (list-ref base-types 1))
          ((string? in)
           (list-ref base-types 2))
          ((integer? in)
           (list-ref base-types 3))
          ((real? in)
           (list-ref base-types 4))
          ((complex? in)
           (list-ref base-types 5))
          ((number? in)
           (list-ref base-types 6))
          ((procedure? in)
           (list-ref base-types 7))
          ((symbol? in)
           (list-ref base-types 8))
          ((char? in)
           (list-ref base-types 9))
          ((pair? in)
          (list-ref base-types 10)))))   

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
          (begin 
            (printit "--- assoc method sig list ---")          
            (printit meth-list)         
            (printit "-- print comp val ---")
            (printit meth-short-sig)
            (let ((method-candidate (assoc  meth-short-sig meth-list)))            
              (if method-candidate              
                  (cdr method-candidate)                
                  #f)))))))

(define make-actual-context  
  (let ((context (make-context #f #f))
        (save-context (make-context #f #f)))
    (lambda ()
      (letrec ((set-context! (lambda (self env)
                               (let ((cont (make-context self env)))
                              (set! save-context context)
                              (set! context cont)
                              cont)))
               (get-context (lambda () context))
               (get-save-context (lambda () save-context)))        
        (vector set-context! get-context get-save-context)))))
(define actual-context (make-actual-context))


(define getsymbol-fortype
  (lambda (obj)
  (let* 
      ((type (assoc obj base-types)))       
    (if (eq? type #f)
        'notfound
        (cdr type)))))
        
              
(define class-type-class 'class)
(define method-type-class 'method)
(define compound-type-class 'compound)
(define prim-type-class 'primitive)
;type-symbol

(define generate-class-sym 
  (lambda (class-anchor-def)
    (generate-sym (class-anchor-name class-anchor-def))))

(define generate-method-sym 
  (lambda (method-def)
    (generate-sym (car method-def))))

(define gentype-symbol 
  (lambda (typeobj typeclass)
    (case typeclass
      ('class (generate-class-sym typeobj))
      ('method (generate-method-sym typeobj))      
      (else #f))))

(define make-type (lambda (typenamesym typeclass typeobj) 
                     (cons typenamesym (list typeclass typeobj))))

(define make-type-registry 
  (lambda ()
    (let ((type-def-list '()))
      (letrec 
          ((add-type 
            (lambda (type-def typeclass) 
              (let* ((type-sym (gentype-symbol type-def typeclass))
                     (type-def-obj (make-type type-sym typeclass type-def)))
                (set! type-def-list (cons 
                                      (cons type-sym type-def-obj) 
                                      type-def-list)) type-sym)))
           (get-type (lambda(class-key) (assoc class-key type-def-list))))
        (vector add-type get-type)))))


(define class-type-registry (make-type-registry))

(define reg-add-class-type 
  (lambda(type-def) 
    ((vector-ref class-type-registry 0) type-def class-type-class)))

(define reg-get-class-type 
  (lambda(class-name) 
    ((vector-ref class-type-registry 1) (generate-sym class-name))))
  

(define class-def 
  (lambda (class-name . more)    
    (let* ((class-extend
           (cond ((null? more) more)
          ((>= (length more) 1) (generate-sym (car more)))))
          (class-interface 
           (cond ((null? more) more)
                 ((>= (length more) 2) (map generate-sym (cdr more)))))
      
          (class-anchor-def 
           (make-class-anchor class-name (generate-sym class-name) class-extend class-interface #f '() '() #f )))
      (reg-add-class-type class-anchor-def)
      class-anchor-def)))

; generate a method symbol as a magling 
;name for the method identifying the method as unique type name
; including the class-name the method name and the method parameter type definition
; put this part of the stuff below in a separate define

(define get-symbolpart-forparms  
  (lambda (plist)
    (if (null? plist) ""
        (let ((type (get-symbolpart-forparms (cdr plist))))
        (string-append (symbol->string (getsymbol-fortype (car plist))) "-" type)))))

(define get-symbolpart-forarguments  
  (lambda (plist)
    (if (null? plist) ""
        (let ((type (get-symbolpart-forarguments (cdr plist))))
        (string-append (symbol->string (getsymbol-fortype-of-data (car plist))) "-" type)))))

(define getsymbol-fortype-of-data
  (lambda (data)
  (let ((type (get-param-type data)))         
    (if (eq? type #f)
        'notfound
        (cdr type)))))


(define build-parmlist  
  (lambda (p-list)  
    (printit "parms -- ")(printit p-list)
    (printit "p --")(printit (car p-list))
    (let ((count 0))      
      (letrec ((loop (lambda (p-list count)
                       (if (null? p-list) '()      
                           (let ((parm-var  (make-variable (number->string count) (number->string count)  #f (car p-list))))
                             ;(printit parm-var)
                             (cons parm-var                             
                                   (loop (cdr p-list) (+ count 1))))))))
        (loop p-list 0)))))
        

(define method-def 
  (lambda (class-name m-name m-proc m-parmtype-list m-ret)
    (let* ((parm-list (build-parmlist m-parmtype-list))               
           (meth-short-sig        
            (generate-sym           
             (string-append                          
              (symbol->string m-name)            
              "_"            
              (get-symbolpart-forparms m-parmtype-list))))            
           (method-sig        
            (generate-sym           
             (string-append              
              (symbol->string class-name)
              "#"
              (symbol->string meth-short-sig))))
           (method-struct (make-method m-name meth-short-sig method-sig #f (make-variable "ret" "ret" #f m-ret) parm-list m-proc))         
           (class-anchor-def (cdr (reg-get-class-type class-name)))           
           (c-anchor (cadr (cdr class-anchor-def))))
      (set-class-anchor-methods! c-anchor      
                                 (cons (cons method-signature method-struct) 
                                       (class-anchor-methods c-anchor)))
      (printit method-struct)
      method-struct)))

(define member-var-def
  (lambda (class-name var-name type)
    (let* ((variable-struct (make-variable var-name var-name #t type))
           (class-anchor-def (cdr (reg-get-class-type class-name)))         
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
      variable-struct)))

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



(define var-cell 
  (lambda (name type?)
    (let ((cell-name name)
          (cell-type-proc? type?)
          (cell-value '()))
      (letrec ((cell-get (lambda () cell-value))
               (cell-set! (lambda (value) (set! cell-value value)))
               (cell-type? (lambda (value) (cell-type-proc? value))))        
        (vector cell-get cell-set! cell-type?)))))

(define make-membervar-cells 
  (lambda (member-list)  
    (printit "cell-maker")
    (printit member-list)
    (if (null? member-list) '()    
        (let* ((var-def (if (pair? (car member-list))                                
                            (cdr (car member-list))))           
               (var-name (variable-name var-def))           
               (var-type? (variable-type? var-def)))              
          (cons (cons var-name (var-cell var-name var-type?))                
                (make-membervar-cells (cdr member-list)))))))
                                 

(define class-getter 
 (lambda (c-sym) 
   (cadr (cddr (reg-get-class-type c-sym)))))


(define get-meth-pair
  (lambda (pair)
    (if (pair? pair)    
        (let ((meth (cdr pair)))          
          (cons (method-name meth) meth))        
        #f)))
    

(define make-normalized-method-list
  (lambda (meth-list)
    (map get-meth-pair meth-list)))
      
      
(define make-object-pool
  (lambda ()
    (let ((pool '((null (null  null)))))
      (letrec ((add-object (lambda (space name obj)
                             
                             (let ((n-space  (assoc space pool)))
                               (let ((add-space (if n-space                                   
                                                    n-space                                   
                                                    (let ((new-space (cons space '())))                                     
                                                      (set! pool (cons new-space pool))
                                                      new-space))))
                                 (set-cdr! add-space (cons (cons name obj) (cdr add-space)))
                                 (printit " --- object pool: ---")(printit pool)                                   
                                 add-space))))
               (get-object (lambda (space name)
                             (printit " search -- ") (printit name)
                             (let ((n-space  (assoc space pool)))
                               (if n-space 
                                   (let* ((obj-list (cdr n-space))
                                          (object (assoc name obj-list)))
                                     object))))))
        (vector add-object get-object)))))         

(define object-pool (make-object-pool))
               
(define c-new 
  (lambda (c-anchor)
    (let* ((super-class-id (class-anchor-super c-anchor))
           (super-class (reg-get-class-type super-class-id))
           (s-obj (if (and (not (null? super-class)) super-class)                      
                      (c-new (class-getter (car super-class)))
                      #f)))  
      
      (let* ((member-list (class-anchor-variables c-anchor))
           
             (member-cell-list (make-membervar-cells member-list))
             (completed-member-cell-list (if super-class ;; Mike Sperber said that's enough                  
                                             (append member-cell-list (selfp-obj-env s-obj))                  
                                             member-cell-list))       
             (meth-list (make-normalized-method-list (class-anchor-methods c-anchor)))
             (completed-meth-list (if super-class
                                      (append meth-list (selfp-obj-vtab s-obj))
                                      meth-list))
             (object (make-selfp (generate-object-ident (class-anchor-qualified-name c-anchor))
                               (class-anchor-qualified-name c-anchor) 
                               c-anchor
                               completed-member-cell-list
                               completed-meth-list ;; later on v-tab selfp-c-vtab
                               s-obj)))
        ((vector-ref object-pool 0) 'global (selfp-obj-ident object) object)
        
      object))))

(define make-method-call
  (lambda (rettype? types)
    (lambda(proc . args)
      (let ((real-args (if (list? (car args))
                           (car args)
                           args)))
      (printit " ------------------  begin of meth - call -----------------------------------")
      
      (printit types)
      
      (printit proc)
      (printit "arguments to check")
      (printit real-args)
                    (printit " ------------------  end of meth - call -----------------------------------")
      (let ((parms-ok-list 
             (map (lambda (p t) (p t)) types real-args)))
        (printit " args count  ")(printit (length args))
        (printit "real args count ")(printit (length real-args))        
        (if (true-list? parms-ok-list)
            (let ((result 
                   (if (and (list? real-args) (eq? (length args) 1))
                       (apply proc real-args)
                       (proc real-args))))
              (if (rettype? result)
                  result
                  'ret-type-error))
            'parm-type-error))))))


(define really-call-method 
  (lambda (selfp-ref meth-ref m-call meth-proc params)
    (dynamic-wind (lambda()
                    (let ((this-pointer (make-this-ref selfp-ref meth-ref #f #f #f #f)))
                      ((vector-ref actual-context 0) this-pointer #f)
                      this-pointer))
                  (lambda () (m-call meth-proc params) (printit ((vector-ref actual-context 1))))
                  (lambda () (let ((save-context ((vector-ref actual-context 2)) ))                    
                               ((vector-ref actual-context 0) (context-this save-context) 
                                                              (context-env save-context))                    
                               (printit 'finished))))))
      
(define send-message
  (lambda (obj-ref message . parms)
    (let*((obj-pair ((vector-ref object-pool 1) 'global (selfp-obj-ident obj-ref)))
         (meth-short-sig (generate-sym           
             (string-append                          
              (symbol->string message)            
              "_"            
              (get-symbolpart-forarguments parms))))
         (check-dummy-method (make-method  message meth-short-sig #f #f #f #f #f)))
      (printit "arg-sig")
      (printit method-short-sig)
      (printit obj-pair)
      (if (pair? obj-pair)
          (let ((object (cdr obj-pair)))
            (if (selfp? object)
                (begin
                (printit message)
                (let ((real-method (check-method-incl-sig check-dummy-method (selfp-obj-vtab object))))
                  (if real-method
                  (let* ((ret-type (method-type real-method))
                         (parm-def (method-parms real-method))
                         (p-type-list (map variable-type? parm-def))
                         (meth-proc  (method-proc real-method))                       
                         (m-call (make-method-call (variable-type? ret-type) p-type-list))
                         (params parms))
                    (printit " ------------------  begin of debug -----------------------------------")
                    (printit (variable-type? ret-type))
                    (printit p-type-list)
                    (printit meth-proc)
                    (printit " ------------------  end of debug -----------------------------------")
                    (really-call-method object real-method m-call meth-proc params)))))
                #f))))))

(define call-super 
  (lambda (params)
    (let* ((this-reference (context-this ((vector-ref actual-context 1))))
           (this-method (this-ref-act-method this-reference)))
      (printit "begin data call-super") 
      (printit this-method)
      (printit this-reference)
      (printit "end data call-super")
      (let* ((object (this-ref-selfp this-reference))            
             (c-super (selfp-obj-super-class object))           
             (s-vtab (selfp-obj-vtab c-super))           
             (real-method (check-method-incl-sig this-method s-vtab)))
      (printit "begin data call-super 2") 
      (printit object)
      (printit s-vtab)
      (printit real-method)
      (printit "end data call-super 2")
      (if real-method          
          (let* ((ret-type (method-type real-method))
                         (parm-def (method-parms real-method))
                         (p-type-list (map variable-type? parm-def))
                         (meth-proc  (method-proc real-method))                      
                         (m-call (make-method-call (variable-type? ret-type) p-type-list)))
            
                    (printit " ------------------  begin of debug call-super-----------------------------------")
                    (printit (variable-type? ret-type))
                    (printit p-type-list)
                    (printit meth-proc)
            
                    (printit " ------------------  end of debug call-super-----------------------------------")
                    (really-call-method object real-method m-call meth-proc params))            
          #f)))))

(define member-var
  (lambda (var-name)
    (let* ((this-reference (context-this ((vector-ref actual-context 1))))
           (this-method (this-ref-act-method this-reference)))
      (printit "begin data member-var") 
      (printit this-method)
      (printit this-reference)
      (printit "end data member-var")
      (let* ((object (this-ref-selfp this-reference))             
             (obj-env (selfp-obj-env object))
             (variable (assoc var-name obj-env)))
        (if variable
            (let ((real-var (cdr variable)))
              ((vector-ref real-var 0)))
            #f)))));raise an error

(define member-var-set!
  (lambda (var-name value)
    (let* ((this-reference (context-this ((vector-ref actual-context 1))))
           (this-method (this-ref-act-method this-reference)))
      (printit "begin data member-var-set!") 
      (printit this-method)
      (printit this-reference)
      (printit "end data member-var-set!")
      (let* ((object (this-ref-selfp this-reference))                          
             (obj-env (selfp-obj-env object))
             (variable (assoc var-name obj-env)))
        (if variable
            (let* ((real-var (cdr variable))                   
                   (var-type? (vector-ref real-var 2)))                  
              (if (var-type? value)                      
                  ((vector-ref real-var 1) value)                      
                  #f)) ; raise illegal type err                  
            #f)))));raise an error
  
  (define new-object 
    (lambda (c-name)
    (c-new (class-getter c-name))))
      


(define-sclass  person
  make-person-obj             
  person?             
  ((name string?) 
   (vorname string?))             
  ((get-name (lambda (num sym) (display num)(newline)"hallo") (list integer? symbol?) string?)))

(define-sclass  employee person 
  make-employee-obj             
  employee?             
  ((status integer?) 
   (abteilung string?))             
  ((get-name (lambda (num sym) (display num)(newline)"hallo") (list integer? symbol?) string?)))

