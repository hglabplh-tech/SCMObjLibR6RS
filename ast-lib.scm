(module ast-lib mzscheme
  (require "objective-s.scm")
  (require (lib "records.ss" "srfi" "57"))
  (require (lib "vector-lib.ss" "srfi" "43"))
  (define union-constructor
    (lambda (name field-defs)
      (let ((field-def (car field-defs)))      
        (let ((fname 
               (cond ((pair? field-def)
                      (car field-def))
                     (else field-def)))
              (ftype 
               (cond ((pair? field-def)
                      (cdr field-def))
                     (else 'none))))
          (cons (cons fname ftype) 
                (union-constructor name cdr field-defs))))))
  
  (define-record-type node-type
    (make-node-type sym name)
    node-type?
    (sym node-type-sym)
    (name node-type-name))
  
  (define node-types (vector 
                      (make-node-type 'app-node "app-node")
                      (make-node-type 'proc-node "proc-node")
                      (make-node-type 'expr-node "expr-node")
                      (make-node-type 'value-node "value-node")))
  
  

  (define-sclass  element  
    make-element-obj  
    ((lambda (in)     
       (display "..init element.. -> ")     
       (display " --- redefined --- ")     
       (display in)     
       (newline) #t))  
    element?
    ((name string? is-public mutable)   
     (parent element? is-public '()  mutable)   
     (type node-type? is-public (vector-ref node-types 0) mutable)   
     (right element? is-public '() mutable)   
     (left element? is-public '() mutable))    
    ((add-element-right is-public    
                        (lambda (elem)                  
                          (member-var-set! element.right element)                  
                          element)                
                        ((elem element? )) element? call-normal)   
     (add-element-left is-public                
                       (lambda (elem)                  
                         (member-var-set! element.left element)                  
                         element)                
                       ((elem element? )) element? call-normal))) 
  
  (define-sclass node element
    make-node-obj
    ((lambda (in)
       (display "node construct")))
    node?
    ((child-list list? '() mutable))
    ((add-child is-public (lambda (child)                           
                            (member-var-set! node.child-list 
                                            (cons child (member-var node.child-list))) 
                            (member-var node.child-list))
               ((child element?)) list? call-normal)))    
  )
