# Lab6

林璐霞 18377149

## Part10

### 实现

* while 语句的实现和if语句几乎一模一样，甚至还简单一点，因为不需要考虑else的跳转。

* while语句`while(cond)stmt`的跳转的翻译为：

* ```
  br label %cond-start
  cond-start:
  br %1 cond label %content-start, label %end
  content-start:
  	...stmt
  	br label %cond-start
  end:
  ```

* 第一句的跳转是因为，不管是while还是if，最后一句都是一个没有内容的llvm块。而while语句的开头需要一个label来标记。为了避免if语句后紧接着的while语句而产生的空块，就加一个没啥用的br语句。

* 其他部分和if几乎一模一样。

## Part11

### 实现

* break语句和continue语句的作用都是跳出循环。break语句是跳到`%end`，continue是跳到 `%cond-start`。而这两个label需要while函数的时候提供给他的stmt语句。因此给Stmt子程序（以及Stmt会调用的子程序）增加了可选的参数`block-start`和`block-end`，这两个参数平时为空，而当while语句调用Stmt的时候，就会将这两个参数传进去。通过递归子程序最终传给`Cont-Stmt`和`Break-Stmt`，由这两个子程序输出`br label %cond-start`和`br label %end`