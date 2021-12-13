lexer.rkt : 词法分析程序
	- lexer 得到token的list

paser.rkt : 语法分析程序

ir-list-generator.rkt : 将paser得到的抽象语法树解析成llvm ir。此时的输出还是一些列表，还带有一些不应该输出的信息，比如类型。

print-llvm-ir.rkt : 将ir-list-generator得到的列表变成llvm ir的方式输出到标准输出流。这时候的应该就是能跑起来的llvm ir代码。

tool.rkt : 分离出来一些小工具函数，比如make-counter。

cal_const.rkt : 这个文件提供的函数主要处理全局变量，比如常数全局变量的注册，全局变量初值的计算等。

grammer:
	lexer   : 
	keywords: sysy中规定的保留字
	all_grammer: 所有的语法，供参考
	op-info.rkt: 从paser得到的类型相对应的llvm的符号等信息。



program_input : 输入的程序默认在这。