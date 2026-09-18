; Example of tree data type

(define-record-scheme <tree #f <tree?) 

(define-record-type (node <tree) make-node node?
  (lhs node.lhs)
  (rhs node.rhs))

(define-record-type (leaf <tree) make-leaf leaf?
  (val leaf.val))

(define (tree->list t)
  (cond
    ((leaf? t) (leaf.val t))
    ((node? t) (cons (tree->list (node.lhs t))
                     (tree->list (node.rhs t))))))

(define t 
  (make-node (make-node (make-leaf 1)
                        (make-leaf 2))
             (make-node (make-node (make-leaf 3) (make-leaf '()))
                        (make-leaf 6))))

(<tree? t)         ;==> #t
(tree->list t)     ;==> ((1 . 2) . 3)


(define test-list (list '(dummy "0")
                   '(a "try-a-1")                        
                   '(b "try-b")                        
                   '(c "try-c")                        
                   '(a "try-a-2")                        
                   '(d "try-d")                        
                   '(e "try-e")                        
                   '(a "try-a-3")                        
                   '(f "try-f")))
