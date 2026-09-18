(module internal-defs mzscheme
  
  (provide 
   
   ;; export class anchor record
   is-public is-protected is-private   
   mutable immutable 
   
   make-class-anchor class-anchor?
   
   class-anchor-name set-class-anchor-name!
   
   class-anchor-qualified-name set-class-anchor-qualified-name!
   
   class-anchor-super set-class-anchor-super!
   
   class-anchor-super-list
   
   class-anchor-interfaces set-class-anchor-interfaces!
   
   class-anchor-constructor set-class-anchor-constructor!
   
   class-anchor-variables set-class-anchor-variables!
   
   class-anchor-pub-methods set-class-anchor-pub-methods!
   
   class-anchor-prot-methods set-class-anchor-prot-methods!
   
   class-anchor-priv-methods set-class-anchor-priv-methods!
   
   class-anchor-finalizer set-class-anchor-finalizer!
   
   
   
   ;; export interface-def record
   
   make-interface-def interface-def?   
   
   interface-def-name set-interface-def-name!
   
   interface-def-qualified-name set-interface-def-qualified-name!
   
   interface-def-variables set-interface-def-variables!
   
   interface-def-methods set-interface-def-methods!    
   
   
   
   ;; export variable record
   
   make-variable variable?
   variable-ident set-variable-ident!
   variable-name set-variable-name!
   variable-value set-variable-value!
   variable-member? set-variable-member?!
   variable-stor-class set-variable-stor-class!
   variable-type? set-variable-type?!
   variable-contract set-variable-contract
   variable-v-class set-variable-v-class
   
   
   
   ;; export method record
   
   make-method method?   
   method-name set-method-name!   
   method-m-class set-method-m-class!   
   method-short-sig set-method-short-sig!   
   method-signature set-method-signature!   
   method-super set-method-super!   
   method-type set-method-type!   
   method-parms set-method-parms!   
   method-proc set-method-proc!
   method-opts set-method-opts!
   method-contract set-method-contract!
   
   
   
   ;; export interface-inst
   
   make-interface-inst interface-inst?
   
   interface-inst-def set-interface-inst-def!
   
   interface-inst-selfp set-interface-inst-selfp!
   
   
   
   ;; export selfp record
   
   
   
   make-selfp selfp?
   
   selfp-obj-ident set-selfp-obj-ident!
   
   selfp-obj-c-type set-selfp-obj-c-type!
   
   selfp-obj-c-anchor set-selfp-obj-c-anchor!
   
   selfp-obj-env set-selfp-obj-env!
   
   selfp-obj-vtab-pub set-selfp-obj-vtab-pub!
   
   selfp-obj-vtab-prot set-selfp-obj-vtab-prot!
   
   selfp-obj-vtab-priv set-selfp-obj-vtab-priv!
   
   selfp-obj-super-obj set-obj-selfp-super-obj!
   
   selfp-result set-selfp-result!
   
   
   
   ;; export this-ref record
   
   
   
   make-this-ref this-ref?
   
   selfp this-ref-selfp 
   
   this-ref-act-method
   
   this-ref-call-super
   
   this-ref-var-get
   
   this-ref-var-set!             
   
   this-ref-var-member?
   
   
   
   ;; export context record
   
   
   
   make-context context?
   
   context-this
   
   context-env
   
   
   ;; var-contract record
   make-var-contract var-contract?    
   var-contract-proc set-var-contract-proc!
   var-contract-params set-var-contract-params!
   
   
   
   ;; export different other bits
   
   printit
   
   build-parmlist
   build-return
   
   generate-class-sym  
   
   generate-method-sym  
   
   gentype-symbol
   
   generate-sym
   
   get-symbolpart-forparms
   
   get-symbolpart-forarguments
   
   getsymbol-fortype
   
   base-types
   
   list-equal?
   
   obj-equal?
   
   make-normalized-method-list
   
   ;; expoerts for the stack definitions   
   stackdef              
   make-stack  
   stack-push!  
   stack-pop  
   stack-init  
   stack-empty?  
   stack-top);; end export
  
  
  
  
  (require (lib "etc.ss"))
  (require (lib "records.ss" "srfi" "57"))
  
  ;; variable for controling real print out
  
  (define really-print #t)
  
  
  (define is-public 'is-public)
  (define is-protected 'is-protected)
  (define is-private 'is-private)
  
  (define mutable 'mutable)
  (define immutable 'immutable)
  
  (define-record-type class-anchor    
    (make-class-anchor name qualified-name super super-list interfaces                       
                       constructor variables pub-methods prot-methods priv-methods finalizer)    
    class-anchor?    
    (name class-anchor-name set-class-anchor-name!)    
    (qualified-name class-anchor-qualified-name                    
                    set-class-anchor-qualified-name!)    
    (super class-anchor-super set-class-anchor-super!)    
    (super-list class-anchor-super-list)    
    (interfaces class-anchor-interfaces set-class-anchor-interfaces!)    
    (constructor class-anchor-constructor set-class-anchor-constructor!) ;; use this field    
    (variables class-anchor-variables set-class-anchor-variables!)    
    (pub-methods class-anchor-pub-methods set-class-anchor-pub-methods!)    
    (prot-methods class-anchor-prot-methods set-class-anchor-prot-methods!)    
    (priv-methods class-anchor-priv-methods set-class-anchor-priv-methods!)    
    (finalizer class-anchor-finalizer set-class-anchor-finalizer!))
  
  
  
  (define-record-type interface-def    
    (make-interface-def name qualified-name variables pub-methods)    
    interface-def?    
    (name interface-def-name set-interface-def-name!)    
    (qualified-name interface-def-qualified-name                    
                    set-interface-def-qualified-name!)    
    ;(super class-anchor-super set-class-anchor-super!)    
    ;(super-list class-anchor-super-list)    
    (variables interface-def-variables set-interface-def-variables!)    
    (pub-methods interface-def-methods set-interface-def-methods!))
  
  
  
  (define-record-type variable
    (make-variable ident name value member? stor-class type? contract v-class)  
    variable?
    (ident variable-ident set-variable-ident!)
    (name variable-name set-variable-name!) 
    (value variable-value set-variable-value!)
    (member? variable-member? set-variable-member?!)
    (stor-class variable-stor-class set-variable-stor-class!)
    (type? variable-type? set-variable-type?!)
    (contract variable-contract set-variable-contract)
    (v-class variable-v-class set-variable-v-class)) 
  
  (define-record-type var-contract
    (make-var-contract proc params)
    var-contract?    
    (proc var-contract-proc set-var-contract-proc!)
    (params var-contract-params set-var-contract-params!))
  
  
  
  (define-record-type method    
    (make-method name m-class short-sig signature super 
                 type parms proc opts contract)    
    method?    
    (name method-name set-method-name!)    
    (m-class method-m-class set-method-m-class!)    
    (short-sig method-short-sig set-method-short-sig!)    
    (signature method-signature set-method-signature!)    
    (super method-super set-method-super!)    
    (type method-type set-method-type!)
    (parms method-parms set-method-parms!)    
    (proc method-proc set-method-proc!)
    (opts method-opts set-method-opts!)
    (contract method-contract set-method-contract!))
  
  
  
  (define-record-type selfp    
    (make-selfp obj-ident obj-c-type obj-c-anchor obj-env                
                obj-vtab-pub obj-vtab-prot obj-vtab-priv obj-super-class result)    
    selfp?    
    (obj-ident selfp-obj-ident set-selfp-obj-ident!)    
    (obj-c-type selfp-obj-c-type set-selfp-obj-c-type!)    
    (obj-c-anchor selfp-obj-c-anchor set-selfp-obj-c-anchor!)    
    (obj-env selfp-obj-env set-selfp-obj-env!)    
    (obj-vtab-pub selfp-obj-vtab-pub set-selfp-obj-vtab-pub!)    
    (obj-vtab-prot selfp-obj-vtab-prot set-selfp-obj-vtab-prot!)    
    (obj-vtab-priv selfp-obj-vtab-priv set-selfp-obj-vtab-priv!)    
    (obj-super-class selfp-obj-super-obj set-obj-selfp-super-obj!)
    (result selfp-result set-selfp-result!)  )
  
  
  
  (define-record-type interface-inst    
    (make-interface-inst def selfp)    
    interface-inst?    
    (def interface-inst-def set-interface-inst-def!)    
    (selfp interface-inst-selfp set-interface-inst-selfp!))  
  
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
            (printit (method-m-class meth)) 
            (printit (method-short-sig meth))            
            (printit (method-signature meth))            
            (printit (method-super meth))            
            (printit (method-type meth))            
            (printit (method-parms meth))            
            (printit (method-proc meth))
            (printit (method-opts meth))
            (printit "--- end print out of method ---")))))  
  
  (define print-variable    
    (lambda (var)      
      (if (variable? var)          
          (begin            
            (printit "--- print out of variable ---")            
            (printit (variable-ident var))
            (printit (variable-name var))
            (printit (variable-value var))
            (printit (variable-member? var))
            (printit (variable-type? var))
            (printit (variable-stor-class var))              
            (printit (variable-contract var))
            (printit (variable-v-class var))  
            (printit "--- end print out of variable ---")))))
  
  (define print-var-contract
    (lambda (contract)       
      (if(var-contract? contract)
         (begin
           (printit "--- print out of variable // contract ---")
           (printit (var-contract-proc contract))
           (printit (var-contract-params contract))
           (printit "--- print out of variable // contract ---")))))
  
  
  
  
  
  
  (define print-selfp    
    (lambda (selfp)      
      (if (selfp? selfp)          
          (begin            
            (printit "--- print out of selfp ---")            
            (printit (selfp-obj-ident selfp))            
            (printit (selfp-obj-c-type selfp))            
            (printit (selfp-obj-c-anchor selfp))            
            (printit (selfp-obj-env selfp))            
            (printit (selfp-obj-vtab-pub selfp))            
            (printit (selfp-obj-vtab-prot selfp))            
            (printit (selfp-obj-vtab-priv selfp))            
            (printit (selfp-obj-super-obj selfp))            
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
            (printit (class-anchor-pub-methods ca))            
            (printit (class-anchor-prot-methods ca))            
            (printit (class-anchor-priv-methods ca))            
            (printit (class-anchor-finalizer ca))            
            (printit "--- end class - anchor - record - print ---") ))))
  
  (define print-interface-def
    (lambda (interf)
      (if (interface-def? interf)
          (begin
            (printit "--- interface definition - record - print ---")
            (printit (interface-def-name interf))    
            (printit (interface-def-qualified-name interf ))    
            (printit (interface-def-variables interf))    
            (printit (interface-def-methods interf))
            (printit "--- end interface definition - record - print ---")))))
  
  (define print-interface-inst
    (lambda (inst)
      (if (interface-def? inst)
          (begin
            (printit "--- interface instance - record - print ---")
            (printit (interface-inst-def inst))    
            (printit (interface-inst-selfp inst))
            (printit "--- end interface instance - record - print ---")))))
  
  
  
  (define print-list-elements    
    (lambda (alist)      
      (if (null? alist)          
          (printit " -- ) --")          
          (begin (printit (car alist))                 
                 (print-list-elements (cdr alist))))))  
  
  ;; main debug print
  
  (define printit    
    (lambda (in)      
      (if really-print          
          (cond ((method? in)                 
                 (print-method in))                
                ((variable? in)                 
                 (print-variable in))
                ((var-contract? in)                 
                 (print-var-contract in))           
                ((class-anchor? in)                 
                 (print-class-anchor in))
                ((interface-def? in)                 
                 (print-interface-def in))                        
                ((interface-inst? in)                 
                 (print-interface-inst in))  
                ((selfp? in)                 
                 (print-selfp in))                
                ((this-ref? in)                 
                 (print-this-ref in))                
                ((context? in)                 
                 (print-context in))                
                ((list? in)                 
                 (begin                   
                   (printit " -- ( --")                   
                   (print-list-elements in)))                
                (else                 
                 (begin                   
                   (display in)                   
                   (newline)))))))
  
  
  
  (define build-return
    (lambda (element)
      (let ((parm-var (cond ((procedure? element)                                                     
                             (make-variable "ret"
                                            "ret" 
                                            #f
                                            #f mutable element #f is-public))                                                  
                            ((pair? element)                                                   
                             (if (and (procedure? (car element))                                                            
                                      (list? (cdr element)))                                                       
                                 (let* ((p-type (car element))                                                              
                                        (contr-list (cdr element))                                                              
                                        (var-contr (make-var-contract                                                                          
                                                    (car contr-list)                                                                          
                                                    (cdr contr-list))))                                                         
                                   (make-variable "ret" "ret" #f                               
                                                  #f mutable p-type var-contr is-public))
                                 'raise-error))                                                       
                            (else 'raise-error))))
        parm-var)))
  
  
  
  (define build-parmlist    
    (lambda (p-list)      
      (printit "parms -- ")(printit p-list)      
      (printit "p --")(if (not '())(printit (car p-list)))
      (let ((count 0))        
        (letrec ((loop (lambda (p-list count)                         
                         (if (null? p-list) '()
                             (let* ((element (car p-list))
                                    (parm-var (cond ((pair? element)                                                   
                                                     (make-variable (car element)                                                            
                                                                    (car element) #f
                                                                    #f mutable (cdr element) #f is-public))
                                                    ((vector? element)
                                                     (if (and (pair? (get-name-pair element)) 
                                                              (list? (get-list element)))
                                                         (let* ((p-name (car (get-name-pair element)))
                                                                (p-type (cdr (get-name-pair element)))
                                                                (contr-list (get-list element))                                                             
                                                                (var-contr (make-var-contract                                                              
                                                                            (car contr-list)                                   
                                                                            (cdr contr-list))))                                                         
                                                           (make-variable p-name
                                                                          p-name #f
                                                                          #f mutable p-type var-contr is-public))
                                                         'raise-error))
                                                    
                                                    (else 'raise-error))))                               
                               (cons parm-var                                     
                                     (loop (cdr p-list) (+ count 1))))))))
          (loop p-list 0)))))
  
  (define get-name-pair 
    (lambda (in)
      (vector-ref in 0)))
  
  (define get-list
    (lambda (in)
      (vector-ref in 1)))
  
  
  
  
  
  (define getsymbol-fortype    
    (lambda (obj)      
      (let* ((type (assoc obj base-types)))        
        (if (eq? type #f)            
            'notfound            
            (cdr type)))))
  
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
  
  
  
  (define generate-sym    
    (lambda (value)      
      (if (null? value)           
          value          
          (if (string? value)              
              (string->symbol value)              
              value))))
  
  
    (define getsymbol-fortype-of-data
    (lambda (data)
      (let ((type (get-param-type data)))         
        (if (eq? type #f)
            'notfound
            (cdr type))))) 
  
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
  
  
   ;;record helper 
  ; some utilities  
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
  
  ;; bits for comparing
  
  ;; compare and equal? procedures 
  
  
  (define comp-classes
    (lambda (c-anchor c-anchor-comp)
      (cond ((not (obj-equal? (class-anchor-name c-anchor) (class-anchor-name c-anchor-comp)))
             #f)
            ((not (obj-equal? (class-anchor-super c-anchor) (class-anchor-super c-anchor-comp)))
             #f)
            ((not (obj-equal? (class-anchor-pub-methods c-anchor) (class-anchor-pub-methods c-anchor-comp)))
             #f)
            (else #t))))
  
  
  (define comp-interf-to-class
    (lambda (c-interf c-anchor)
      (cond ((not (obj-equal? (interface-def-variables c-interf) (class-anchor-variables c-anchor) #t))
             #f)
            ((not (obj-equal? (interface-def-methods c-interf) (class-anchor-pub-methods c-anchor) #t))
             #f)
            (else #t))))
  
  
  (define comp-variable-interf
    (lambda (var var-comp)
      (cond ((not (obj-equal? (variable-name var) (variable-name var-comp)))     
             #f)    
            ((not (obj-equal? (variable-type? var) (variable-type? var-comp)))
             #f)
            (else #t))))
  
  (define comp-method-interf 
    (lambda (meth meth-comp)
      (cond     ((not (obj-equal? (method-name meth) (method-name meth-comp)))
                 #f)                    
                ((not (obj-equal? (method-short-sig meth) (method-short-sig meth-comp)))     
                 #f)    
                (else #t))))
  
  
  
  (define comp-selfp
    (lambda (obj obj-comp)
      (cond ((not (obj-equal? (selfp-obj-ident obj) (selfp-obj-ident obj-comp)))
             #f)
            ((not (obj-equal? (selfp-obj-c-type obj) (selfp-obj-c-type obj-comp)))
             #f)
            ((not (obj-equal? (selfp-obj-super-obj obj) (selfp-obj-super-obj obj-comp)))
             #f)
            (else #t))))
  
  (define comp-variable
    (lambda (var var-comp)
      (cond ((not (obj-equal? (variable-ident var) (variable-ident var-comp)))
             #f)    
            ((not (obj-equal? (variable-name var) (variable-name var-comp)))     
             #f)    
            ((not (obj-equal? (variable-type? var) (variable-type? var-comp)))
             #f)
            (else #t))))
  
  (define comp-method 
    (lambda (meth meth-comp )
      (cond     ((not (obj-equal? (method-name meth) (method-name meth-comp)))
                 #f)    
                ((not (obj-equal? (method-m-class meth) (method-m-class meth-comp)))     
                 #f)    
                ((not (obj-equal? (method-short-sig meth) (method-short-sig meth-comp)))     
                 #f)    
                (else #t))))
  
  
  ;; if the compare is used for interfaces the first list must be the list of the interface
  
  (define list-equal?
    (opt-lambda (alist list-comp (interf-comp #f))
      (if interf-comp 
          (list-equal-memq? alist list-comp)
          (list-equal-by-elements? alist list-comp))))
  
  
  (define list-equal-by-elements?
    (opt-lambda (alist list-comp)
      (if (null? alist) 
          #t      
          (if (not (eq? (length alist) (length list-comp)))          
              #f          
              (if (not (obj-equal? (car alist) (car list-comp)))              
                  #f              
                  (and #t (list-equal-by-elements? (cdr alist) (cdr list-comp))))))))
  
  
  (define list-equal-memq?
    (lambda (alist list-comp)   
      (if (null? alist) 
          #t
          (if (not (memq-obj (car alist)  list-comp #t))
              #f              
              (and #t (list-equal-memq? (cdr alist) list-comp))))))
  
  (define memq-obj 
    (opt-lambda (element alist (interf-comp #f))
      (if (null? alist) 
          #f   
          (let ((value (cdr element))
                (c-value (cdar alist)))
            (printit "values to compare are")
            (printit value)
            (printit c-value)
            (if (obj-equal? value c-value interf-comp)                
                alist
                (memq-obj element (cdr alist) interf-comp))))))
  
  (define obj-equal?
    (opt-lambda (first second
                       (interf-comp #f))
      (let ((comp-meth-proc (if interf-comp
                                comp-method-interf
                                comp-method))                                
            (comp-var-proc (if interf-comp 
                               comp-variable-interf
                               comp-variable)))
        (cond ((and (class-anchor? first) (class-anchor? second))
               (comp-classes first second))
              ((and (list? first) (list? second))
               (list-equal? first second interf-comp))
              ((and (interface-def? first) (class-anchor? second))
               (comp-interf-to-class first second))
              ((and (selfp? first) (selfp? second))
               (comp-selfp first second))
              ((and (method? first) (method? second))
               (comp-meth-proc first second))
              ((and (interface-def? first) (interface-def? second))
               #f)
              ((and (variable? first) (variable? second)) 
               (comp-var-proc first second))             
              ((and (symbol? first) (symbol? second))
               (eq? first second))
              (else (equal? first second))))))
  
  
  
  ;; build up utilities around method handling
  (define get-meth-pair
    (lambda (pair)
      (if (pair? pair)    
          (let ((meth (cdr pair)))          
            (cons (method-name meth) meth))        
          #f)))
  
  (define-syntax stackdef (
                           syntax-rules()
                            ((stackdef name                                  
                                       (push! pop empty? init top))
                             (begin
                               (define name (make-stack 'name))
                               (define push! (stack-push! name))
                               (define pop (stack-pop name))
                               (define empty? (stack-empty? name))
                               (define init (stack-init name))
                               (define top (stack-top name))
                               ))))  
  
  (define make-stack
    (lambda (name)  
      (let ((s '()))
        (letrec 
            ((empty? (lambda () (null? s)))
             (top (lambda () (if (not (empty?)) (car s) s)))
             (push! (lambda (x) (set! s (cons x s))))
             (pop (lambda () (if (empty?) 'err          
                                 (let ((top (car s)))
                                   (set! s (cdr s))
                                   top))))
             (initialize (lambda () (set! s '())
                           'done)))    
          (vector push! pop initialize empty? top)))))
  
  (define stack-push! 
    (lambda (stack) 
      (lambda (value)
        ((vector-ref stack 0) value))))
  
  (define stack-pop 
    (lambda (stack) 
      (lambda ()
        ((vector-ref stack 1)))))
  
  (define stack-init 
    (lambda (stack) 
      (lambda ()
        ((vector-ref stack 2)))))
  
  (define stack-empty?
    (lambda (stack) 
      (lambda ()
        ((vector-ref stack 3)))))
  
  (define stack-top
    (lambda (stack) 
      (lambda ()
        ((vector-ref stack 4)))))
  
  (define make-normalized-method-list
    (lambda (meth-list)
      (map get-meth-pair meth-list)))
  );end of module




