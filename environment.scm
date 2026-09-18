(module environment mzscheme  
  (provide genaddr-tab
           addrtab? 
           addrtab-empty?
           addrtab-size
           addrtab-peek
           addrtab-add
           addrtab-latest
           addrtab-find
           addrtab-remove-latest
           addrtab-locateval
           addrtab-locatesym
           addrtab-add-val
           var-cell
           cell-get  
           cell-set!     
           cell-type?  
           cell-get-name  
           cell-get-class  
           cell-get-v-class
           cell-get-type?
           make-membervar-cells
           make-parmlist-env
           make-object-pool
           pool-add-object
           pool-get-object
           get-local
           decl-var)
  
  (require "internal-defs.scm"
           (lib "etc.ss")
           (lib "records.ss" "srfi" "57")
           "o-exceptions.scm" 
           rnrs/mutable-pairs-6   
           rnrs/lists-6)
  
  (define-record-type name-space
    (make-name-space name elements)
    name-space?
    (name name-space-name set-name-space-name!)
    (elements name-space-elements set-name-space-elements!))
  
  
  
  (define var-cell
    (opt-lambda (name type? v-class (value '??value??) (stor-class mutable))
      (let ((cell-name name)
            (cell-type-proc? type?)
            (cell-v-class v-class)
            (cell-value '??value??)
            (cell-class stor-class))
        (letrec ((cell-get (lambda () cell-value))
                 (cell-set! (lambda (value) (if (and (eq? cell-class mutable)
                                                     (cell-type-proc? value))
                                                (set! cell-value value)) #t))
                 (cell-set-intern! (lambda (value) (if (cell-type-proc? value)
                                                       (set! cell-value value))))
                 (cell-type? (lambda (value) (cell-type-proc? value)))
                 (cell-get-type? (lambda () cell-type-proc?))
                 (cell-get-name (lambda () cell-name))
                 (cell-get-class (lambda () cell-class))
                 (cell-get-v-class (lambda() cell-v-class)))
          
          (cell-set-intern! value)
          (vector cell-get cell-set! cell-type? cell-get-name cell-get-class cell-get-v-class cell-get-type? )))))
  
  (define cell-get
    (lambda (cell) ((vector-ref cell 0))))
  
  (define cell-set!
    (lambda (cell value) ((vector-ref cell 1) value)))
  
  (define cell-type?
    (lambda (cell) ((vector-ref cell 2))))
  
  (define cell-get-name
    (lambda (cell) ((vector-ref cell 3))))
  
  (define cell-get-class
    (lambda (cell) ((vector-ref cell 4))))
  
  (define cell-get-v-class
    (lambda (cell) ((vector-ref cell 5))))
  
  (define cell-get-type?
    (lambda (cell) ((vector-ref cell 6))))
  
  (define make-membervar-cells 
    (lambda (member-list)  
      (printit "cell-maker")
      (printit member-list)
      (if (null? member-list) '()    
          (let* ((var-def (if (pair? (car member-list))                                
                              (cdr (car member-list))))           
                 (var-name (variable-name var-def))
                 (var-value (variable-value var-def))
                 (var-type? (variable-type? var-def))
                 (v-class (variable-v-class var-def))
                 (var-stor-class (variable-stor-class var-def))
                 (var-contract (variable-contract var-def)))                     
            (cons (cons var-name (var-cell var-name var-type? v-class var-value var-stor-class))                
                  (make-membervar-cells (cdr member-list)))))))
  
  (define make-parmlist-cells 
    (lambda (member-list value-list)  
      (printit "cell-maker")
      
      (if (null? member-list) '()
          (begin
            
            (let* ((var-def (car member-list))                                       
                   (var-name (variable-name var-def))
                   (var-value (car value-list))
                   (var-type? (variable-type? var-def))
                   (var-stor-class (variable-stor-class var-def))
                   (var-contract (variable-contract var-def)))
              (cons (var-cell var-name var-type? is-public var-value var-stor-class)
                    (make-parmlist-cells (cdr member-list) (cdr value-list))))))))
  
  (define make-parmlist-env
    (lambda (env)
      (lambda (member-list value-list)
        (let* ((cell-list (make-parmlist-cells member-list value-list))
               (names (map cell-get-name cell-list)))
          (addrtab-add env names cell-list) cell-list))))
  
  (define get-local
    (lambda (addrtab)
      (lambda (sym)    
        (let ((address (addrtab-find addrtab sym))) ;; fails in addrtab-find see hat happens env seems to be a bit shuffled
          (printit address)
          (if address
              (let ((depth (car(cdr address))))
                (if (eq? depth 0)
                    (let ((cell (addrtab-locateval addrtab (cdr address))))
                      (cell-get cell)))))))))
  
  (define decl-var 
    (opt-lambda (sym type? addrtab (init-val '??value??)(cell-class mutable))
      (let ((var-decl (var-cell sym type? init-val cell-class)))
        (addrtab-add-val addrtab sym var-decl))))
  
  
  
  
  (define find-field (lambda (fieldlist fieldname)
                       (letrec ((innerfind 
                                 (lambda (counter)                                             
                                   (if (eq? counter (vector-length fieldlist)) 
                                       (raise (make-cond &invalid-type-cond
                                                         (string-append "identifier does not exist: " 
                                                                        (symbol->string fieldname))))
                                       (if (eq? (vector-ref fieldlist counter) fieldname)
                                           counter
                                           (innerfind (+ counter 1))))))) (innerfind 0))))
  
  (define find-address (lambda (name table)
                         (letrec (( inner (lambda (depth tab)
                                            (if (null? tab) 
                                                (raise (make-cond &invalid-type-cond
                                                                  (string-append "identifier does not exist: " 
                                                                                 (symbol->string name))))
                                                (let ((position (find-field
                                                                 (car (car tab)) name)))
                                                  (if (not(eq? position -1)) 
                                                      (cons name (list depth position))
                                                      (inner (+ depth 1) (cdr tab)))))))) (inner 0 table))))
  (define genaddr-tab
    (lambda ()
      (let ((type-id 'addrtab)
            (tabs '()))
        (letrec
            ((empty? (lambda() (null? tabs)))
             (size  (lambda() (length tabs)))
             (peek (lambda(pos) (vector-ref(list->vector tabs) pos)))
             (add (lambda (syms exp)
                    (set! tabs (cons  (cons
                                       (list->vector syms)
                                       (list->vector exp)) tabs))))
             (add-val (lambda (sym obj)
                        (let* ((latest (car tabs))
                               (syms-list (vector->list(car latest)))
                               (vals-list (vector->list (cdr latest))))                    
                          (set-car! latest (list->vector (append syms-list (list sym))))
                          (set-cdr! latest (list->vector (append vals-list (list obj)))))))  
             (locateval
              (lambda(address)
                (locate address cdr)))
             (locatesym
              (lambda(address)
                (locate address car)))
             (locate
              (lambda(address fun)
                (if (pair? address)
                    (let ((depth (car address))
                          (pos   (cadr  address)))
                      (if (and (< depth (length tabs))
                               (>= depth 0))
                          (let ((addrvect (fun (list-ref tabs depth))))
                            (if (and (< pos (vector-length addrvect))
                                     (>= depth 0))
                                (vector-ref addrvect pos)
                                #f))
                          #f)))))
             (latest (lambda () (car tabs)))
             (find (lambda (name)(find-address name tabs)))
             (remove-latest (lambda () (if (not (null? tabs)) (set! tabs (cdr tabs))))))
          (vector empty? size peek add latest find remove-latest locateval locatesym add-val type-id)))))
  
  
  (define addrtab? (lambda (table) (eq? (vector-ref table 10) 'addrtab)))
  (define addrtab-empty? (lambda (table) (let ((func (vector-ref table 0))) (func))))
  
  (define addrtab-size (lambda (table) (let ((func (vector-ref table 1))) (func))))
  
  (define addrtab-peek (lambda (table pos) (let ((func (vector-ref table 2))) (func pos))))
  
  (define addrtab-add (lambda (table symlist addrlist ) (let ((func (vector-ref table 3))) 
                                                          (func symlist addrlist))))
  
  (define addrtab-latest (lambda (table) (let ((func (vector-ref table 4))) (func))))
  
  (define addrtab-find (lambda (table name) (let ((func (vector-ref table 5))) (func name))))
  
  (define addrtab-remove-latest (lambda (table) (let ((func (vector-ref table 6))) (func))))
  
  (define addrtab-locateval (lambda (table address) (let ((func (vector-ref table 7))) (func address))))
  
  (define addrtab-locatesym (lambda (table address) (let ((func (vector-ref table 8))) (func address))))  
  
  (define addrtab-add-val (lambda (table sym obj) (let ((func (vector-ref table 9))) (func sym obj))))
  
  (define make-object-pool
    (lambda ()
      (let ((pool (list)))
        (letrec ((add-object (lambda (space name obj)                               
                               (let ((n-space  (find (lambda (e) 
                                                       (eq? (name-space-name e) space)) pool)))
                                 (let ((add-space (if n-space                                   
                                                      n-space                                   
                                                      (let ((new-space (make-name-space space '())))
                                                        (set! pool (mcons new-space pool))
                                                        new-space))))
                                   (set-name-space-elements! add-space (mcons (mcons name obj) 
                                                                              (name-space-elements add-space)))                                 
                                   add-space))))
                 (get-object (lambda (space name)
                               (let ((n-space  (find (lambda (e) 
                                                       (eq? (name-space-name e) space)) pool)))
                                 (if n-space 
                                     (begin 
                                       (let* ((obj-list (name-space-elements n-space))
                                              (object (assoc name obj-list)))
                                         (cons (mcar object) (mcdr object)))))))); this is done to give the stuff back immutable
                 (remove-object (lambda (space name)
                                  (let ((object (get-object space name))
                                        (n-space  (assoc space pool)))
                                    (if object
                                        (rm-pair-from-list (cdr n-space) (car object)))))))                                  
          (vector add-object get-object remove-object)))))
  
  
  (define pool-add-object 
    (lambda (pool) 
      (lambda (name-space name obj)
        ((vector-ref pool 0) name-space name obj))))
  
  (define pool-get-object 
    (lambda (pool) 
      (lambda (name-space name)
        ((vector-ref pool 1) name-space name))))
  
  (define pool-remove-object 
    (lambda (pool) 
      (lambda (name-space name)
        ((vector-ref pool 2) name-space name))))
  
  
  (define rm-pair-from-list 
    (lambda (node-list node-name)    
      (cond ((null? node-list) '())
            ((eq? (caar node-list) node-name)
             (cdr node-list))
            (else (cons 
                   (car node-list)
                   (rm-from-list 
                    (cdr node-list) node-name))))))
  
  (define rm-from-list 
    (lambda (node-list element)    
      (cond ((null? node-list) '())
            ((eq? (car node-list) element)
             (cdr node-list))
            (else (cons 
                   (car node-list)
                   (rm-from-list 
                    (cdr node-list) element))))))
  )


