((
	VarDef 
	#(struct:token Ident "a") 
	(#(struct:token Assign "=") (InitVal Exp AddExp (MulExp (UnaryExp PrimaryExp . #(struct:token Number 0)) ()) ()))))