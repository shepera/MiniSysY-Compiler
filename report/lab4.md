# Lab4

林璐霞 18377149

- 简要介绍，你在完成这个 lab 中的各个 part 时都做了哪些工作，为什么要用（你采用的）这种方法完成。
- 实验指导中每个 part 中留下的思考题的回答。（鼓励但不强制，这不会成为评分依据）
- 如果你复用或者借鉴了参考代码或其他实现（关于借鉴与抄袭的界定请见诚信页），请明确写出你借鉴了哪些内容。



## Part7

### 实现

实现条件语句主要有两部分：布尔表达式，和if语句的跳转

* 布尔表达式：

  * 由于布尔表达式中需要进行与或非的运算，因此现在的编译器需要两种类型了：i1和i32。

    * 首先需要记住每个llvm ir标示符的类型。之前`ir-list-generator`函数得到的是所有语句的列表，每个语句都是一个列表表示的。注意到大部分语句开头都是"%x1"等，因此现在每个表示语句的列表开头加上了"%x1"的类型，（如果不是"%x1"开头，则加一个void）。得到的语句如：`i32 %x16 = add i32 %x14 , %x15 `。在最后输出的时候才把类型去掉。
    * 接着还要实现i32和i1之间的类型转换。i32的`%x1`到i1的实现是判断`%x1`是否等于0；i1到i32的实现是利用了zext语句。

  * 接着实现逻辑表达式。由于之前使用了`generate-ir-expr `函数统一完成了加法和乘法，而布尔表达式的与和或的语句的形式都和加法语句很相似，因此直接可以直接通过完善该函数得到。另外，保存了op-hash，对每个操作符记录了操作数类型，返回值类型，该符号在llvm ir中应该是哪个符号。

    ```lisp
    (define (generate-ir-expr op num1 num2 counter)
      ; to generate ir code for expressions like : 1 + 2
      (define type-info (hash-ref op-hash op))
      
      ;该运算符在llvm ir中的表示
      (define op-ir (operation-ir type-info)) 
      ; 操作数类型
      (define type-needed (operation-operand-type type-info))
      ; 返回值类型
      (define ret-type (operation-ret-type type-info))
      
      ; 对 num1 和 num2 做一次类型转换
      (define cast-num1 (type-cast num1 type-needed counter))
      (define cast-num2 (type-cast num2 type-needed counter))
      
      (append
       (get-code cast-num1) ;得到num1的前置代码
       (get-code cast-num2) ;得到num2的前置代码
       (list (flatten (list
                       ret-type
                       (get-llvm-var counter)
                       "="
                       op-ir type-needed
                       (get-value cast-num1)
                       ","
                       (get-value cast-num2)))))) ;增加表达式
    ```

    

* if跳转

  if语句的实现就很方便了。先得到两个stmt的内容。

  如果有else语句，即 if (cond) stmt1 else stmt2的形式，则变成

  ```
  br cond, label 1, label 2
  label 1:
  	stmt1
  label 2:
  	stmt2
  label 3:
  ```

  否则变成：

```
br cond, label 1, label 2
label 1:
	stmt1
label 2:
```