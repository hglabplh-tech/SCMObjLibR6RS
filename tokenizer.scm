(module tokenizer mzscheme
  (provide string-tokenizer
           parse           
           gettokfrom
           processloop
           is-in-list?)
          
 
  ;; system requirements
  (require (lib "string.ss" "srfi" "13"))  
  (require (lib "records.ss" "srfi" "57"))
  (require (lib "vector-lib.ss" "srfi" "43"))
  
  ;; project requirements
  (require "ast-lib.scm")
  (require "internal-defs.scm" )
  
  (define-record-type keyword-def
    (make-keyword-def keyword type context)
    keyword-def?
    (keyword keyword-def-keyword)
    (type keyword-def-type)
    (context keyword-def-context))
  
  (define is-in-list? (lambda (char slist) (letrec ((loop (lambda (plist) 
                                                            (if (eq? char (car plist)) 
                                                                #t                                                                              
                                                                (cond ((pair? (cdr plist))                                     
                                                                       (loop (cdr plist)))
                                                                      (else #f))))))
                                             (loop slist))))
  
  (define string-tokenizer
    (lambda (tokstr delim)
      (let ((act-token '())
            (tokens '()))
        (letrec ((gettoken (lambda (tokstr delim k)
                             (let ((tok-list (string->list tokstr)))                          
                               (letrec ((strtokint                           
                                         (lambda (tok-list delim cont)
                                           (let ((token (if (null? tok-list)
                                                            " "
                                                            (car tok-list))))
                                             (cond   ((null? tok-list) 
                                                      (cont tok-list))                                          
                                                     ((is-in-list? (car tok-list) delim)
                                                      (strtokint (cdr tok-list) delim                                                        
                                                                 (lambda (mylist)
                                                                   (set! tokens (cons (list->string (reverse act-token)) tokens))
                                                                   (set! act-token '())                                                 
                                                                   (cont (cdr tok-list)))))
                                                     (else
                                                      (strtokint (cdr tok-list) delim 
                                                                 (lambda (mylist)                                                        
                                                                   (set! act-token  (append act-token (list (car tok-list))))
                                                                   (append (cont tok-list) (list (car tok-list)))                                 
                                                                   tokens))))))))
                                 (strtokint tok-list delim k))))))
          (gettoken tokstr delim (lambda (in) 
                                   (set! tokens 
                                         (cons (list->string (reverse act-token)) 
                                               tokens)) 
                                   tokens))))))
  
  (define parse
    (lambda (tokstr delim)      
      (let ((tok-list '()))                                           
        (string-fold
         (lambda (c token)
           (if (not (is-in-list? c delim))               
               (set! token (append token (list c)))                                                  
               (let ((act-token (list->string token)))
                 (set! token '())
                 (set! tok-list (cons  act-token tok-list))
                 (call-with-current-continuation
                  (lambda (cont)
                    (tokenize act-token cont '())))))               
           token)
         '() tokstr)
        (reverse tok-list))))  
  
  (define rule-patterns
    (list (cons 'if (cons 'sequence (list 'expression 'then (cons 'expression 'block) 'else ( cons 'expression 'block))))
          (cons 'expression (list 'expression 'block))))
          
  (define first-token "program") 
  (define main-valid-tokens
    (list (cons "program" (make-keyword-def 'program 'block '(block)))
          (cons "main" (make-keyword-def 'main 'block '(block)))
          (cons "in" (make-keyword-def 'in 'block '(block)))
          (cons "if" (make-keyword-def 'if 'condition '(block)))
          (cons "then" (make-keyword-def 'then 'condition '(if)))       
          (cons "else" (make-keyword-def 'else 'condition '(then)))
          (cons "do" (make-keyword-def 'do 'condition '(if)))
          (cons "end" (make-keyword-def 'end 'statement '(block)))
          (cons "class" (make-keyword-def 'class 'block '(block)))
          (cons "interface" (make-keyword-def 'class 'block '(block)))
          
          (cons "method" (make-keyword-def 'method 'block '(block)))))
  
  
  (define tokenize 
    (lambda (token cont cont-parm)
      (let ((tok-pair (assoc token main-valid-tokens)))
        (if (pair? tok-pair)
            (set-context tok-pair)
            (begin (display "::bullshit::")(newline)))
        (display "->token::")(newline)
        (display token)(newline)
        (cont cont-parm))))
  
  (define set-context
    (lambda (tok-pair)      
      (let ((token (cdr tok-pair)))      
        (let* ((keyword (keyword-def-keyword token))
               (context (assoc keyword rule-patterns)))
          (if context
              (begin (display context)(newline))
              (display "no context available"))        
          (display keyword)        
          (display " --> ")        
          (display (keyword-def-context token))(newline)))))
  
  (define gettokfrom (lambda(tokenizer index) 
                       (letrec (( gettok (lambda (tokenizer indcount)                    
                                           (if (= indcount  index) 
                                               (cond((pair? tokenizer)  (car tokenizer)) 
                                                    (else null))                                               
                                               (cond ((not (null? tokenizer)) 
                                                      (gettok (cdr tokenizer) (+ indcount 1))) 
                                                     (else null))))))                       
                         (gettok tokenizer 0))))
  (define processloop (lambda (toklist ) (letrec ((getnext 
                                                   (lambda (counter)                                                
                                                     
                                                     (if (null? (gettokfrom toklist counter)) 
                                                         'processed
                                                         
                                                         (begin 
                                                           (display (gettokfrom toklist counter))                                    
                                                           (newline)
                                                           (getnext (+ counter 1)))))))                                         
                                           (getnext 0))))
  
  
  
  )
