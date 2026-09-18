;;make this a module
(module objective-s mzscheme
  (provide define-sclass
           define-sinterface
           send-message
           send-message-by-names
           send-message-internal
           call-super
           self$
           redefine-constructor
           is-public is-protected
           is-private  
           mutable immutable
           is-obj-for-interface?
           check-memq check-memv
           make-checked-interface-instance
           printit member-var 
           member-var-set!
           call-by-name
           call-normal
           get-local-var
           call-construct)
  
  
  ;; objective extension for mzscheme and 
  ;; later on scheme48 author : Harald Glab-Plhak
  (require "objectfactory.scm" )
  
  ;; define-syntax for classes which are a compound object out of member variables, methods and super - classes
  
  (define-syntax (define-sclass sclass-stx)
    (define generateid
      (lambda (template-id . args)
        (datum->syntax-object template-id
                              (string->symbol
                               (apply string-append
                                      (map (lambda (x)
                                             (if (string? x)
                                                 x
                                                 (symbol->string
                                                  (syntax-object->datum  x))))
                                           args))))))
    (syntax-case sclass-stx ()       
      ((_ class-name make-obj (constructor) predicate?
          ((var-name  var-pred? v-class . v-more) ...)
          ((meth-name meth-class proc ((name type . more) ...)  ret-pred? call-op) ...))
       (with-syntax  (((method-set ...)
             (map (lambda (x) (generateid x (syntax class-name) "."  x))
                  (syntax->list (syntax (meth-name ...)))))
            ((var-set ...)
             (map (lambda (x) (generateid x (syntax class-name) "."  x))
                  (syntax->list (syntax (var-name ...)))))
            
            )        
         (syntax 
          (begin
            (define class-name (class-def 'class-name))             
            (define make-obj (lambda () (new-object 'class-name)))
            (redefine-constructor class-name constructor)
            (define predicate? (isofclass class-name))
            (begin (define var-set 'var-name) (apply member-var-def (var-type class-name var-name var-pred? v-class . v-more))) ...
            (begin (define method-set 'meth-name)(apply method-def  (list 'class-name 'meth-name meth-class proc 
                                                                         (list (parm-type name type . more) ...) ret-pred? call-op))) ...))))
      ((_ class-name super-class make-obj (constructor) predicate? 
          ((var-name  var-pred? v-class . v-more) ...)
          ((meth-name meth-class proc ((name type . more) ...) ret-pred? call-op) ...))
       (with-syntax  (((method-set ...)
             (map (lambda (x) (generateid x (syntax class-name) "."  x ))
                  (syntax->list (syntax (meth-name ...)))))
            ((var-set ...)
             (map (lambda (x) (generateid x (syntax class-name) "."  x ))
                  (syntax->list (syntax (var-name ...)))))
            
            )                   
         (syntax                 
          (begin
            (define class-name (class-def 'class-name 'super-class)) 
            (define s-class 'super-class)
            (define make-obj (lambda () (new-object 'class-name)))
            (redefine-constructor class-name constructor)
            (define predicate? (isofclass class-name))        
            (begin (define var-set 'var-name) (apply member-var-def (var-type class-name var-name var-pred? v-class . v-more))) ...         
            (begin (define method-set 'meth-name)(apply method-def  (list 'class-name 'meth-name meth-class proc 
                                                                         (list (parm-type name type . more) ...) ret-pred? call-op)))...))))))
  
  ;;define-syntax - helper to define methods for classes
  
  (define-syntax parm-type
    (syntax-rules ()    
      ((_ name type)
       (begin
         (cons 'name type)))
      ((_ name type proc . more)
       (begin
         (vector (cons 'name type) `(,proc . more))))))
  
  ;;define-syntax - helper to define member - variables for classes
  
  (define-syntax var-type
    (syntax-rules ()    
      ((_ c-name var-name  var-pred? v-class)    
       (begin
         (list 'c-name 'var-name var-pred? v-class)))
      ((_ c-name var-name  var-pred?  v-class value)    
       (begin
         (list 'c-name 'var-name var-pred?  v-class value)))
      ((_ c-name var-name  var-pred?  v-class value stor-class)    
       (begin
         (list 'c-name 'var-name var-pred?  v-class value stor-class)))
      ((_ c-name var-name  var-pred?  v-class value stor-class proc . more)    
       (begin
         (list 'c-name 'var-name var-pred?  v-class value stor-class `(,proc . more))))))    
  
  
  ;;define-syntax for defining interfaces - interfaces do not have any real methods themselve but 
  ;; describe the access to a class and are able to receive a class assignment
  
  (define-syntax define-sinterface
    (syntax-rules ()       
      ((_ interface-name 
          ((var-name var-pred?) ...)
          ((meth-name ((name type . more) ...) ret-pred?) ...)
          predicate?)       
       (begin
         (define interface-name (interf-def 'interface-name 
                                            (list (list 'var-name var-pred?) ...)
                                            (list (list 'meth-name 'interface-name (list (parm-type name type . more) ...) ret-pred?) ...)))
         (define predicate? (isofclass interface-name))))))
  
  ;;define-syntax for send-message - send-message sends a message to a given object
  
  (define-syntax send-message
    (syntax-rules ()
      ((_ object msg . more)
       (begin
         (send-message-internal object msg . more)))))
  
  (define-syntax send-message-by-names
    (syntax-rules ()
      ((_ object msg (name value) ...)
       (begin
         (send-message-internal-call-by-name object msg (cons 'name value) ...)))))
  
  ;;define-syntax for call-super - call-super calls the 'super' of a method but only if there is a method in 
  ;; the super class which has a signature that fits the need
  
  (define-syntax call-super
    (syntax-rules ()  
      ((_  first ...)
       (begin
         (call-super-internal (list first ...))))))
  )
