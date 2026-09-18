((:alnum: (<set> [<seq> (#\A  #\Z)][<seq> (#\a  #\z)] [<set> #\ä #\ö #\ü #\Ä #\Ö #\Ü #\ß ]))
 (:numeric: ([<seq> (#\0 #\9)]))
 (entry  [:ident:])
 (:ident: (<set> [:numeric:] [:alnum:])))