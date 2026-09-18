(load "./utils.ss")
(define genlex-addrs (lambda (parml depth)(letrec ((nextp(lambda (parml depth counter result) (if (null? parml) (begin(display result)(newline)  result)
                                                                                                  ( nextp (cdr parml) depth (+ counter 1) (append result (list (cons (car parml) (list depth #\: counter))))))))) (nextp parml depth 0 '()))))


;; change this stuff especially the conversion from list->vector see gen-addr-tab
(define find-address (lambda (name table)
                       (letrec (( inner (lambda (depth tab) 
                                          (if (null? tab) -1
                                              (let ((position (find-field 
                                                               (car (car tab)) name)))                               
                                                (if (not(eq? position -1)) (cons name (list depth position)) 
                                                    (inner (+ depth 1) (cdr tab)))))))) (inner 0 table))))

(define genaddr-tab-org 
  (lambda () 
    (let ((tabs '()))
      (lambda (cmd)
        (case cmd
          ((empty?) (lambda() (null? tabs)))
          ((size?) (lambda() (length tabs)))
          ((peek) (lambda(pos) (vector-ref(list->vector tabs) pos)))
          ((add) (lambda (exp)                     
                   (set! tabs (cons  (list->vector exp) tabs))))           
          ((latest) (lambda () (car tabs)))            
          ((find) (lambda (name)(let ((table (list->vector tabs))) (find-address name table))))
          ((remove-latest) (lambda () (if (not (null? tabs)) (set! tabs (cdr tabs)))))           
          (else (error "invalid message" cmd)))))))

(define genaddr-tab 
  (lambda () 
    (let ((tabs '()))
      (letrec      
          ((empty? (lambda() (null? tabs)))
           (size  (lambda() (length tabs)))
           (peek (lambda(pos) (vector-ref(list->vector tabs) pos)))
           (add (lambda (syms exp)                     
                  (set! tabs (cons  (list  (list->vector syms) 
                                           (list->vector exp)) tabs))))
           (locateval 
            (lambda(address)
              (locate address cadr)))
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
        (vector empty? size peek add latest find remove-latest locateval locatesym)))))

;; example of code 

(define parsestring '
  ((defvar (test 5) (try 7))
   (func term (a b)
         (+ a b))
   (term test try)))
(define keywords '(func defvar))
(define keyword? (lambda(in) 
                   (display "val --- ")(display in)(newline)
                   (memv (car in) keywords)))
(define parser 
  (lambda (input)
    (letrec ((addresses (genaddr-tab))
             (loop 
              (lambda (input)
                (if (null? input) 'finished
                    (let ((nextcmd (car input)))
                      (cond ((keyword? nextcmd)
                             (process-keyword nextcmd)))
                      (loop (cdr input)))))))
             (loop input))))

(define process-keyword 
  (lambda (cmd)
    (cond ((eq? (car cmd) 'func) 
           (let ((funname (cadr cmd)))
             (set! keywords (cons funname keywords)))))))
(parser parsestring)
(display keywords)(newline)
             
;;(vector empty? size peek add latest find remove-latest) 
(define addrtab-empty? (lambda (table) (let ((func (vector-ref table 0))) (func))))
(define addrtab-size (lambda (table) (let ((func (vector-ref table 1))) (func))))
(define addrtab-peek (lambda (table pos) (let ((func (vector-ref table 2))) (func pos))))
(define addrtab-add (lambda (table symlist addrlist ) (let ((func (vector-ref table 3))) (func symlist addrlist))))
(define addrtab-latest (lambda (table) (let ((func (vector-ref table 4))) (func))))
(define addrtab-find (lambda (table name) (let ((func (vector-ref table 5))) (func name))))
(define addrtab-remove-latest (lambda (table) (let ((func (vector-ref table 6))) (func))))
(define addrtab-locateval (lambda (table address) (let ((func (vector-ref table 7))) (func address))))
(define addrtab-locatesym (lambda (table address) (let ((func (vector-ref table 8))) (func address))))
(define genfunctab ())
(define addrtab (genaddr-tab))
(addrtab-empty? addrtab) ;;empty?
(addrtab-size addrtab) ;;size?
(addrtab-add  addrtab '(a b c d) '(a9 b8 c56 d88)) ;;add
(addrtab-empty?  addrtab) ;;empty?
(addrtab-size  addrtab) ;;size?

(addrtab-add  addrtab '(a z t k e) '(a6 z45 t10 k8 e7));;add
(display "locate")(newline)
(addrtab-locateval addrtab '(1 3))
(addrtab-locatesym addrtab '(1 3))
(addrtab-latest  addrtab);;latest
(addrtab-peek addrtab 0);;peek
(addrtab-peek addrtab 1);;peek
(addrtab-locateval addrtab (cdr (addrtab-find addrtab 'a)));;find
(addrtab-find addrtab 'c);;find
(addrtab-find addrtab 't);;find
(addrtab-find addrtab 't);;find
(addrtab-find addrtab 'l);;find
(addrtab-size addrtab)
(addrtab-remove-latest addrtab) ;; remove
(addrtab-latest  addrtab);;latest
(addrtab-find addrtab 'c);;find
(addrtab-find addrtab 't);;find
(addrtab-size addrtab)
(define testit (make-vector 4 7))
(define testcopy (list testit))
(define restit (list->vector testcopy))
(vector-set! testit 0 10)
(vector-ref restit 0)
(car testcopy)


                          

  


