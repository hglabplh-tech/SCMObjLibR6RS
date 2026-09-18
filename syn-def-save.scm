(define-syntax (define-sclass sclass-stx)
  (syntax-case sclass-stx ()       
    ((_ class-name make-obj (constructor) predicate?
        ((var-name var-pred?) ...)
        ((meth-name meth-class proc ((name type . more) ...)  ret-pred?) ...))
     (with-syntax ()          
       (syntax
        (begin
          (define class-name (class-def 'class-name))             
          (define make-obj (lambda () (new-object 'class-name)))
          (redefine-constructor class-name constructor)
          (define predicate? (isofclass class-name))
          (begin (define var-name 'var-name) (apply member-var-def (list 'class-name 'var-name var-pred?))) ...
          (begin (define meth-name 'meth-name)(apply method-def  (list 'class-name 'meth-name meth-class proc 
                                                                       (list (parm-type name type . more) ...) ret-pred?))) ...))))
    ((_ class-name super-class make-obj (constructor) predicate? 
        ((var-name var-pred?) ...)
        ((meth-name meth-class proc ((name type . more) ...) ret-pred?) ...))
     (with-syntax ()           
       (syntax                 
        (begin
          (define class-name (class-def 'class-name 'super-class)) 
          (define s-class 'super-class)
          (define make-obj (lambda () (new-object 'class-name)))
          (redefine-constructor class-name constructor)
          (define predicate? (isofclass class-name))
          (begin (define var-name 'var-name) (apply member-var-def (list 'class-name 'var-name var-pred?))) ...
         (begin (define meth-name 'meth-name)(apply method-def  (list 'class-name 'meth-name meth-class proc 
                                                                       (list (parm-type name type . more) ...) ret-pred?)))...))))))


(define-syntax define-sclass 
  (syntax-rules ()       
    ((_ class-name make-obj (constructor) predicate?
        ((var-name var-pred?) ...)
        ((meth-name meth-class proc ((name type . more) ...)  ret-pred?) ...))     
        (begin
          (define class-name (class-def 'class-name))             
          (define make-obj (lambda () (new-object 'class-name)))
          (redefine-constructor class-name constructor)
          (define predicate? (isofclass class-name))
          (begin (define var-name 'var-name) (apply member-var-def (list 'class-name 'var-name var-pred?))) ...
          (begin (define meth-name 'meth-name)(apply method-def  (list 'class-name 'meth-name meth-class proc 
                                                                       (list (parm-type name type . more) ...) ret-pred?))) ...))
    ((_ class-name super-class make-obj (constructor) predicate? 
        ((var-name var-pred?) ...)
        ((meth-name meth-class proc ((name type . more) ...) ret-pred?) ...))      
        (begin
          (define class-name (class-def 'class-name 'super-class)) 
          (define s-class 'super-class)
          (define make-obj (lambda () (new-object 'class-name)))
          (redefine-constructor class-name constructor)
          (define predicate? (isofclass class-name))
          (begin (define var-name 'var-name) (apply member-var-def (list 'class-name 'var-name var-pred?))) ...
         (begin (define meth-name 'meth-name)(apply method-def  (list 'class-name 'meth-name meth-class proc 
                                                                       (list (parm-type name type . more) ...) ret-pred?)))...))))
