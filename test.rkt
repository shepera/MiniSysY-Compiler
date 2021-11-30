((CompUnit 
	(FuncDef 
		(FuncType #(struct:token Int "int")) 
			#(struct:token Ident "main") 
			#(struct:token LPar "(") 
			#(struct:token RPar ")") 
			(Block 
				#(struct:token LBrace "{") 
				(Stmt 
					#(struct:token Return "return") 
					#(struct:token Number 123) 
					#(struct:token Semicolon ";")) 
				#(struct:token RBrace "}")))))