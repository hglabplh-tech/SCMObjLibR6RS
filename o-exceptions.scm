;; objective extension  - 
;; exceptions of the add on itself
;; author : Harald Glab-Plhak
(module o-exceptions mzscheme
  (provide
   make-cond
   &o-error
   o-error-msg
   o-error?
   &invalid-type-cond
   &invalid-identifier-cond)
  (require (lib "condition.ss" "srfi" "35"))
  
  
  (define-condition-type &o-error &error    
    o-error?
    (message o-error-msg))
  
  (define-condition-type &invalid-type-cond &o-error    
    invalid-type-cond?
    (s-message inv-type-msg))
  
    
  (define-condition-type &invalid-identifier-cond &o-error    
    invalid-identifier-cond?    
    (s-message inv-id-msg))
  

  (define make-cond 
    (lambda (cond msg)
      (condition (cond  (s-message "spec") (message  msg)))))
  
  )