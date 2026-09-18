#lang mzscheme
(let ((c (lambda (x a)  
    (display a) (newline)
    (if (< a 10)
        (x x (+ a 1))))))
      (c c 1))

(define test-cont 
  (lambda(t cont)        
    (cont (+ t 2))))
(define continue 
  (lambda (t)
    (display t)
    (newline)
    (test-cont t (lambda (r) 
                   (display (+ r 4))
                   (newline)))))
(test-cont 1 continue) 
                    
