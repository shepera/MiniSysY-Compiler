(Exp 
	AddExp 
		(MulExp 
			(UnaryExp 
				(UnaryOp . #(struct:token Minus "-"))
			(UnaryExp 
				(UnaryOp . #(struct:token Minus "-")) 
				(UnaryExp (UnaryOp . #(struct:token Minus "-")) 
				(UnaryExp PrimaryExp #(struct:token LPar "(")
				(Exp AddExp (MulExp (UnaryExp 
				(UnaryOp . #(struct:token Minus "-"))
				(UnaryExp PrimaryExp . #(struct:token Number 1)))) ()) 
				#(struct:token RPar ")")))))) ())

(Exp 
	AddExp 
		(MulExp 
			(UnaryExp
				PrimaryExp . 
					#(struct:token Number 1))) 
		((#(struct:token Plus "+") (MulExp (UnaryExp PrimaryExp . #(struct:token Number 2))))))

(list 
	(token 'LPar "(") 
	(list 'Exp 'AddExp 
		(list 'MulExp 
			(list 'UnaryExp 
				(cons 'UnaryOp 
					(token 'Minus "-")) 
				(list* 'UnaryExp 'PrimaryExp (token 'Number 1)))) 
		'()) 
	(token 'RPar ")"))

(	
	(MulExp 
		(UnaryExp 
			(UnaryOp . #(struct:token Minus "-")) (... ... ...)) ()) 
	())

(AddExp 
	(MulExp 
		(UnaryExp 
			(UnaryOp . #(struct:token Minus "-")) (... ... ...)) ()) 
	())