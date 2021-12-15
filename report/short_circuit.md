# short_circuit

18377149 林璐霞

* 这个理清思路之后还挺简单的。需要变动的部分极小。

* 修改范围在`If-Stmt`, `While-Stmt`, `Cond`, `LOrExp`, `LAndExp`。

* 短路求值的实现大概就是增加了一堆跳转语句来实现的。

* 顺便一提。。。由于实验指导放在了part之后，我全部通关之后，才发现原来有实验指导这东西。。。我的if和while的实现没有使用回填，而且也没有用数字编号。我直接在if和while中生成了所有需要的编号，需要的话往下传（比如传给Continue语句和break语句）。

* 首先观察到，我们使用condition的时候，总是需要指明如果结果正确跳转到哪，错误跳转到哪。我们将condition为真需要跳转的块叫做`true-block`, `false-block`。之前的没有短路的情况下，我们只需要在if和while语句中判断，然后跳转就行了，但现在需要在每一次and和or的语句中进行跳转。因此做的第一个改变是为`Cond`, `LOrExp`, `LAndExp`增加了两个新的参数`true-block`, `false-block`。调用关系是：`if`/`while`$\to$`cond`$\to$`LOrExp`$\to$​​​`LAndExp`。`true-block`和`false-block`会从if和while开始依次往下传。

* 现在可以删除if和while语句中对条件的判断和跳转了，所有的都在And和Or中完成。

* 然后做的修改是，给and和Or的每个子语句增加跳转。

  * And：

  * 跳转方向大概就像下面这样

  * 可以注意到，所有and子语句当为假的时候，就跳到父函数传给他的`false-block`中；为真就跳到下一块中，除了最后一个表达式，如果为真，就跳转到跳到父函数传给他的`true-block`中

  * ```
    A&&B&&C:   paras: true-block false-block
    
    
       ┌───────┐ if true ┌───────┐ if true  ┌───────┐if true  ┌──────────┐
       │Block A├────────►│Block B├─────────►│Block C├────────►│true-block│
       └───────┤         └──┬────┘          └──┬────┘         └──────────┘
               │            │                  │
               │            │                  │
               │  if false  │        ┌─────────▼─┐
               └────────────┴───────►│false-block│
                                     └───────────┘
    ```

  * 

    ```lisp
    (define (LAndExp ast symbols counter true-block false-block)
      ; LAndExp -> EqExp { '&&' EqExp }
      ; 得到每个and的部分（EqExp为根结点的ast）
      (define and-list 
        (cons 
         (first ast) 
         (map second (second ast))))
      ; 生成一个列表，是每个EqExp为真的时候需要跳转的block编号
      (define true-list 
        (append
          (build-list 
           (sub1 (length and-list)) 
           (lambda (x) (get-llvm-block-id counter)))
          (list true-block)))
      ; 得到EqExp的llvm翻译
      (define exps
        (map (lambda (x)
               (type-cast (EqExp (cdr x) symbols counter) 'i1 counter))
             and-list))
      
      (append (append*
               '()
               (map
                (lambda (exp true-block block-id)
                  (append
                   ; 除了第一个EqExp外，每个EqExp都需要一个块。这里的输出块编号
                   (if (empty? block-id) 
                       '() 
                       (list (list 'void (string-append block-id ":"))))
                   (append
                    ; EqExp的需要的llvm代码
                    (get-code exp)
                    ; 跳转语句，为真跳转到true-list里保存的对应块，为假则是统一的的false-block
                    (list (list 'void 'br 'i1 (get-value exp) ","
                                'label (string-append "%" true-block) "," 
                                'label (string-append "%" false-block))))))
                exps true-list (cons '() (drop-right true-list 1))))))
    ```

  * Or：

  * 几乎和And一模一样。

  * 区别在于，在其下的每个表达式，如果为真，则都跳转到父函数传给他的true-block中，否则跳转到下一个表达式。