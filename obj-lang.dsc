((delimiters  (#\( #\) #\, #\; #\space))
 (operands  (#\+ #\- #\* #\/ #\^))
 (:keywords:  (program main in if then else do end class interface method var for-each recurs let let-recur continuation))
 (:alnum: (<set> [<seq> (#\A  #\Z)][<seq> (#\a  #\z)] [<set> #\ä #\ü #\ü #\Ä #\Ö #\Ü #\ß ]))
 (:numeric: ([<seq> (#\0 #\9)]))
 (:ident: (<set> [:numeric:] [:alnum:]))
 (entry  [:program:])
 (:program:  (main (<or> [:block:]  [:expression:] [:keywords:]) end-pgm))
 (:blockcontent: (<or> [:keywords:]  [:expression:]))
 (:block:  (do  (<or> [:blockcontent:] (<recur> [:blockcontent:] [:block:])) end))
 (:expression: (<or> [:proc-call:] (<recur> [:proc-call:] [:expression:])))
 (:proc-call: (#\( [:proc-name:] [:parmlist:] #\)))
 (:proc-name: ([:ident:]))
 (:parmlist: ([:parm:] (<recur> [:parm:] [:parmlist:])))
 (:parm: (<or> [:ident:] [:value:])))



