#lang mzscheme
(define write-stuff
  (lambda (obj qualified-fname)
    (let ((port (open-output-file qualified-fname)))  
      (letrec 
          ((real-io (lambda (obj port)
                      (if (null? obj)                                          
                            'ready            
                          (let ((portion (car obj)))                            
                            (if (or (pair? portion) (list? portion))                            
                                    (begin (write-char #\newline port)                            
                                           (write-char #\tab port)                                                                                      
                                           (write-char #\( port)                                           
                                           (real-io portion port)
                                           (write-char #\) port))
                                    (begin (write-char #\space port)(write portion port)))
                            (real-io (cdr obj) port))))))
        (real-io obj port))
      (close-output-port port))))
(define exampl (list (list 'try 'test 'west 1 "uli" (list 'try)) (list 'by 'the 'way 2 "bla")))
(write-stuff exampl "./testw.scm")
