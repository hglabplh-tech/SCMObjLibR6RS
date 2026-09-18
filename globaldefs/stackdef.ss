(module stackdef mzscheme
  (provide stackdef ;; high level
           stack-push!
           stack-pop
           stack-init
           stack-top
           stack-empty?          
           stack?
           make-stack) ;; low level
(define-syntax stackdef (
                         syntax-rules()
                          ((stackdef name                                  
                                     (push! pop top init empty?))
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
        (vector (vector (cons 'signature 'stack) (cons 'name name))
                (vector push! pop initialize empty? top))))))
(define gettypevect (lambda(obj) (vector-ref obj 0)))
(define getfuncvect (lambda(obj) (vector-ref obj 1)))
(define stack-push! 
  (lambda(obj)
    (if (stack? obj)
        (lambda (value) 
          (let (( func (vector-ref (getfuncvect obj)  0))) 
            (func value))))))
(define stack-pop (lambda(obj) (if (stack? obj) (lambda () (let ((func (vector-ref (getfuncvect obj)  1))) (func))))))
(define stack-init (lambda(obj) (if (stack? obj) (lambda () (let ((func (vector-ref (getfuncvect obj)  2))) (func))))))
(define stack-empty? (lambda(obj) (if (stack? obj) (lambda () (let ((func (vector-ref (getfuncvect obj)  3))) (func))))))
(define stack-top (lambda(obj) (if (stack? obj) (lambda () (let ((func (vector-ref (getfuncvect obj)  4))) (func))))))
(define stack? (lambda (obj)
                 (let* ((typevect (gettypevect obj)) (typesig (vector-ref typevect 0))) 
                   (and 
                    (eq? (vector-length obj) 2) (pair? typesig)(eq? (car typesig) 'signature) (eq? (cdr typesig) 'stack))
                   )))
)