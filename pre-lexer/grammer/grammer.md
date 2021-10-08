Token 名称	对应字符串	输出格式	备注
标识符	（定义见下）	Ident($name)	将 $name 替换成标识符对应的字符串
无符号整数	（定义见下）	Number($number)	将 $number 替换成标识符对应的字符串
if	if	If	
else	else	Else	
while	while	While	
break	break	Break	
continue	continue	Continue	
return	return	Return	
赋值符号	=	Assign	
分号	;	Semicolon	
左括号	(	LPar	
右括号	)	RPar	
左大括号	{	LBrace	
右大括号	}	RBrace	
加号	+	Plus	
乘号	*	Mult	
除号	/	Div	
小于号	<	Lt	
大于号	>	Gt	
等于号	==	Eq	
错误	不能符合上述 token 规则的字符串	Err	程序应输出 Err 后终止


Letter -> 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's'
    | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L'
    | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'

Digit -> '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

Underline -> '_'

Nondigit -> Letter | Underline

<标识符> -> Nondigit | <标识符> Nondigit | <标识符> Digit

<无符号整数> -> Digit | <无符号整数> Digit