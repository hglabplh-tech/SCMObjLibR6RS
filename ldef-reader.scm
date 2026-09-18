#lang mzscheme
;; (C) Harald Glab-Plhak
;; a litle parser for a language modeling scheme for defining syntax rules
;; and the grammar of a general language
(require mzlib/etc)
(require (lib "vector-lib.ss" "srfi" "43"))
(require "internal-defs.scm")

(stackdef ctx-stack (entity-stack-push! entity-stack-pop entity-stack-empty? entity-stack-init entity-stack-top))
(entity-stack-init)

(define get-lang-def-list
  (let ((def-list '()))
    (opt-lambda ((new-list '()))
      (if (not (null? new-list))      
          (set! def-list new-list))          
      def-list)))

(define get-actual-entity
  (let ((act-entity '()))
    (opt-lambda ((new-entity '()))
      (if (not (null? new-entity))      
          (set! act-entity new-entity))          
      act-entity)))

(define get-actual-cmd
  (let ((act-cmd '()))
    (opt-lambda ((new-cmd '()))
      (if (not (null? new-cmd))      
          (set! act-cmd new-cmd))          
      act-cmd)))

(define read-stuff
  (lambda (port)
    (let ((thing (read port)))
      (if (not (eof-object? thing))
          (begin (display "read thing ----> ")(newline)                 
                 (display (get-lang-def-list thing))(newline)
                 (recurs-parser thing '())
                 (read-stuff port))))))

(define recurs-parser
  (let ((first-p #t))
    (opt-lambda (thing (last-cmd '()) (last-entity '()))        
      (cond (first-p
             (let ((main-entry (assoc 'entry thing)))             
               (if (not main-entry)                 
                   (error 'exit)                 
                   (begin (set! first-p #f)                          
                          (display "main -entry ==>") 
                          (display (caadr main-entry))
                          (newline)                          
                          ;; set the pop stack of a entity after each get-entity recursive call and look
                          ;; if the number of calls can be minimized
                          (let ((res (recurs-parser (get-entity (caadr main-entry)) last-cmd (caadr main-entry))))
                            (display "pop entity --> ")
                            (display (entity-stack-pop))(newline)
                            res)))))
            
            ((not thing) #f)
            ((list? thing)                      
             ;; check if first part is a cmd             
             (display (process-list-action thing last-cmd last-entity))(newline))
            ;((process-list recurs-parser) thing last-cmd last-entity))           
            ((symbol? thing)
             (let ((check-result (check-thing thing)))
               (cond ((eq? (car check-result) 'cmd)
                      (let ((command (cdr check-result)))
                        (get-actual-cmd command)))
                     ((eq? (car check-result) 'entity)         
                      (let ((new-entity (get-entity (cdr check-result))))
                        (if (and new-entity (not (eq? last-entity thing)))
                            (let ((res (recurs-parser  new-entity (get-actual-cmd) thing)))
                              (display "pop entity --> ")(display (entity-stack-pop))(newline)
                              (display " rep stack top ent: --> " )(display (entity-stack-top))(newline)                              
                              res)
                            (begin                            
                              (display "recursive decl for ") 
                              (display thing)(newline)))))
                     (else 'nothing))))))))


(define get-entity 
  (lambda(thing)
    (let ((ent-list (assoc thing (get-lang-def-list))))
      (if ent-list
          (let* ((entity (cadr ent-list))            
                 (entity-name (car ent-list)))                  
            (if (or (not (pair? (entity-stack-top))) (not (eq? entity-name (car (entity-stack-top)))))
                (begin (entity-stack-push! (cons entity-name entity))
                       (display "push entity --> ")(display entity-name)(newline)
                       entity)                
                (begin  (display "entity recursion blocked") '())))))))

(define lang-cmds '#( <and> <or> <set> <recur> <seq>)) 

(define make-member-of-vector?  
  (lambda (vect)
    (let ((vector-set vect))
      (lambda (in)  
        (let ((index  (vector-index (lambda (first)
                                      (cond ((eq? first in) #t)                                               
                                            (else #f))) vector-set)))      
          index)))))

(define lng-cmd?  (make-member-of-vector?  lang-cmds))  

(define process-list-action 
  (opt-lambda (a-list (last-cmd '()) (last-entity '()))
    (if (pair? a-list)    
        (let* ((thing (car a-list))
               (check-result (check-thing thing)))
          (cond ((eq? (car check-result) 'cmd)      
                 (let ((command (cdr check-result)))            
                   (case command              
                     ('<seq> (workout-sequence (cadr a-list)))              
                     ('<set> (get-set-of (cdr a-list)))              
                     ('<or> (create-cond-list (cdr a-list)))
                     ('<and> (create-cond-list (cdr a-list)))
                     ('<recur> (create-cond-list (cdr a-list))))))
                ((eq? (car check-result) 'entity)
                 (let ((new-list (get-entity (cdr check-result))))
                   (if (not (null? new-list))
                       (let ((res (process-list-action new-list last-cmd last-entity)))
                         (display " pla stack top ent: --> " )(display (entity-stack-top))(newline)
                         (display "pop entity --> ")
                         (display (entity-stack-pop))(newline)                     
                         res)
                       (cdr a-list))))
                ((eq? (car check-result) 'syms)
                 (process-list-action (cdr a-list) last-cmd last-entity))                 
                ((eq? (car check-result) 'list)
                 (process-list-action thing last-cmd last-entity))
                ((eq? (car check-result) 'chars)
                 (process-list-action (cdr a-list) last-cmd last-entity))                  
                (else '())))
        '())))

(define create-cond-list
  (lambda (a-list)
    (if (not (list? a-list))
        'wrong-param
        (if (null? a-list)
            '()
            (let* ((what (check-thing (car a-list)))      
                   (whatcode (car what))
                   (whatdata (cdr what)))
              (cond ((eq? whatcode 'chars)
                     (cons whatdata (create-cond-list (cdr a-list))))               
                    ((eq? whatcode 'list)
                     (let ((result (process-list-action whatdata)))
                       (append result (create-cond-list (cdr a-list)))))))))))




(define get-set-of 
  (lambda (a-list) 
    (if (null? a-list)
        '()    
        (let* ((what (check-thing (car a-list)))      
               (whatcode (car what))
               (whatdata (cdr what)))
          (cond ((eq? whatcode 'chars)
                 (cons whatdata (get-set-of (cdr a-list))))               
                ((eq? whatcode 'list)
                 (let ((result (process-list-action whatdata)))
                   (append result (get-set-of (cdr a-list))))))))))



(define workout-sequence 
  (lambda (list)
    (cond ((char? (car list))          
           (let* ((op1 (char->integer (car list))) 
                  (op2 (char->integer(cadr list)))
                  (fun (if (> op1 op2) - +))
                  (cmp-op (if (>= op1 op2) >= <=)))
             (map integer->char(make-char-seq fun cmp-op op1 op2))))
          (else 'invalid-seq))))

(define make-char-seq    
  (lambda (fun cmp-op op1 op2)      
    (if (cmp-op op1 op2)                
        (cons op1 (make-char-seq fun cmp-op (fun op1 1) op2))
        '())))


(define check-thing 
  (lambda (in) 
    (cond ((lng-cmd? in)           
           (cons 'cmd in))
          ((entity? in)           
           (cons 'entity in))
          ((char? in)
           (cons 'chars in))
          ((symbol? in)
           (cons 'syms in))
          ((list? in)
           (cons 'list in))
          (else  (cons 'else '())))))

(define entity?
  (lambda (thing)
    (if (symbol? thing)
        (let ((thing-str (symbol->string thing)))          
          (if (equal? (substring thing-str 0 1) ":")
              #t
              #f))
        #f)))

(define process-list
  (lambda (proc)
    (opt-lambda (in (last-cmd '()) (last-entity '()))  
      (if (null? in) 
          'ret
          (begin 
            (proc (car in) last-cmd last-entity)
            ((process-list proc)(cdr in) last-cmd last-entity))))))

(display (workout-sequence (list #\0 #\9)))
(display (workout-sequence (list #\a #\z)))
(display (workout-sequence (list #\A #\Z)))
(substring "test" 0 1)
(lng-cmd? '<or>)
(lng-cmd? 'mist)
(call-with-input-file "obj-lang.dsc" read-stuff)
