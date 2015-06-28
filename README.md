
###Kaleidoscope++的范式：

1.  top范式表示所有输入

		top ::= definition | external | expression | ';'

1.  toplevelexpr范式

		toplevelexpr ::= expression

2. expression范式

		expression
			::= unary binoprhs

3. unary范式, 首先判断当前的cur_tok是否为unary operator

		unary
			::= primary
			::= op unary

4. binoprhs范式,

		binoprhs
			::=
			::= ('bi_op' unary)*

5. primary范式，主要表示式

		primary
		   ::= identifierexpr
		   ::= numberexpr
		   ::= arrayexpr
		   ::= parenexpr
		   ::= ifexpr
		   ::= forexpr
		   ::= varexpr


6. numberexpr定义数值对象

		numberexpr
			::= double
			::= integer

7. arrayexpr范式，定义一个数组
		
		arrayexpr
			:: = '[' expression* ']'

7. parenexpr括号包围的表达式

		parenexpr ::= '(' expression ')'


8. 如果cur_tok为identifier, 则对象可以是变量或者是函数

	    identifierexpr
    		   ::= identifier
    		   ::= identifier '(' expression* ')'

9. def后跟随的是函数原型定义和函数体表达式, 或是变量;P-

		definition	::= 'def' prototype expression
					::= 'def' identifier expression


10. prototype可以是函数或是binary operator,  unary operator的定义
如果是binary定义，必须定义binary operator的优先度

		prototype
	    	::= id '((parameter,)*)'
	    	::= binary LETTER number? (parameter, parameter)
	    	::= unary LETTER (parameter)

11. parameter声明范式，分别为声明基本类型，一个数组，以及一个指向基本元素的指针

		parameter
			::= type id
			::= type[integer] id
			::= type[] id



12. type范式
		type
			::= 'int'
			::= 'char'
			::= 'double'

11. external 范式声明外部函数, 以extern关键词开始

		external ::= 'extern' prototype

12. ifelseexpr 条件表达式

		ifexpr ::= 'if' expression 'then' expression 'else' expression

13. forexpr 条件循环语句

		forexpr ::= 'for' identifier '=' expr ','  (identifier ‘=’ expr)* ;expr (',' expr)? 'in' expression

14. 定义变量语句, 以var起始,  定义一个或是多个变量，以‘in’结束定义，这些变量被使用与‘in’后跟随的expression中

		varexpr ::= 'var' identifier ('=' expression)?(',' identifier ('=' expression)?)* 'in' expression

15. global语句，定义全局变量

		globalexpr ::= 'global' (type) var_name = expr



###Kaleidoscope+

1. 增加了垃圾回收功能机制
所有的节点在分配内存时即进行登记注册

2.
