(module types mzscheme
  (provide
   make-type-registry
   reg-add-class-type
   reg-get-class-type)
  
  (require "internal-defs.scm")
  
  (define class-type-class 'class)
  (define method-type-class 'method)
  (define compound-type-class 'compound)
  (define prim-type-class 'primitive)
  ;type-symbol
  
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
  
  
  
  
  (define reg-add-class-type 
    (lambda(registry type-def) 
      ((vector-ref registry 0) type-def class-type-class)))
  
  (define reg-get-class-type 
    (lambda(registry class-name) 
      ((vector-ref registry 1) (generate-sym class-name))))
  )