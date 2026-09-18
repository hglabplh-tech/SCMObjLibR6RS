;; objective extension for mzscheme and 
;; later on scheme48 author : Harald Glab-Plhak

(module obj-interfaces mzscheme
  (provide interface-def?
           interf-def
           is-class-for-interface?
           is-obj-for-interface?
           make-checked-interface-instance)
  
  (require "internal-defs.scm")
  (require "environment.scm")
  (require (lib "etc.ss"))
  
  ;(require (lib "records.ss" "srfi" "57"))  
  
  ; to test environment import remove it later !!!!!
  (define this-addrtab (genaddr-tab)) 
  (define interf-def-b
    (lambda (name var-list meth-list)
      (printit var-list)
      (printit meth-list)))

  
   (define interf-def
    (lambda (name var-list meth-list)
      (printit var-list)
      (printit meth-list)
      (let ((vars (map mk-var var-list))
            (meths (map mk-meth meth-list)) 
            (q-name (string->symbol (string-append 
                                     (symbol->string name) 
                                     (symbol->string 'global)))))
        (printit "results vars , meths")
        (printit vars)
        (printit meths)
        (make-interface-def name q-name vars meths))))
  
  (define mk-var 
    (lambda (var-list)
      (apply def-iface-var var-list)))
  
  (define mk-meth
    (lambda (meth-list)
      (apply def-iface-meth meth-list)))
  
  (define def-iface-var    
    (opt-lambda (var-name type 
                            (value '??value??) (stor-class mutable)  (contract #f) (v-class is-public))
      (cons var-name (make-variable var-name var-name value #t stor-class type #f v-class))))
  
  (define def-iface-meth
    (lambda (m-name m-interface m-parmtype-list m-ret) 
         (let* ((parm-list (build-parmlist m-parmtype-list))               
             (meth-short-sig        
              (generate-sym           
               (string-append                       
                (symbol->string m-name)            
                "_"            
                (get-symbolpart-forparms (map variable-type? parm-list)))))            
             (method-sig        
              (generate-sym           
               (string-append              
                (symbol->string m-interface)
                "#"
                (symbol->string meth-short-sig)))))
           
      
           (cons m-name 
                 (make-method m-name m-interface meth-short-sig 
                              method-sig #f (make-variable "ret" "ret" #f #f mutable m-ret #f is-public) m-parmtype-list #f #f #f)))))
  
  (define is-class-for-interface?
    (lambda (interf c-anchor)
      (obj-equal? interf c-anchor)))
  
   (define is-obj-for-interface?
    (lambda (interf object)
      (obj-equal? interf (selfp-obj-c-anchor object))))
  
  (define is-instance-of-interf
    (lambda (interf)
      (lambda (interf-inst)
        ;; i-def is for further use
        (let* ((i-def (interface-inst-def interf-inst))
              (i-selfp (interface-inst-selfp interf-inst))
              (c-anchor (selfp-obj-c-anchor i-selfp)))
          (obj-equal? interf c-anchor)))))
  
  
  
  (define make-checked-interface-instance
    (lambda (obj interf)      
      (if (is-obj-for-interface? interf obj)
          (let ((interf-inst (make-interface-inst interf obj)))            
            interf-inst)
          #f))) ;; raise exception
      
            
          
          
          
  
  
  );; end of module
