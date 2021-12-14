#lang racket

(require "./parser.rkt")
(require "./lexer.rkt")
(require "./tools.rkt")
(require "./grammer/op-info.rkt")
(require "./cal-const.rkt")
(require racket/set)
(provide ir-list-generator)

; get namespace for eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (elem-eval elem)
  (eval elem ns))

(struct sym (name id type feat)  #:transparent)
(struct num-feat (const)  #:transparent)
(struct func-feat (ret para)  #:transparent)


(define lib-func (hash 	"getint" (sym "getint" "@getint" 'function (func-feat 'i32 '()))
                        "putint"  (sym "putint" "@putint" 'function (func-feat 'void '(i32)))
                        "getch"  (sym "getch" "@getch" 'function (func-feat 'i32 '()))
                        "putch"  (sym "putch" "@putch" 'function (func-feat 'void '(i32)))
                        "getarray" (sym "getarray" "@getarray" 'function (func-feat 'i32 '((ptr))))
                        "putarray" (sym "putarray" "@putarray" 'function (func-feat 'void '(i32 (ptr))))
                        "memset" (sym "memset" "@memset" 'function (func-feat 'void '(i32* i32 i32)))))
                      
(define func-include (mutable-set))

(define (get-global-var name)
  (string-append "@" name))

(define (get-llvm-var counter)
  (string-append "%x" (number->string (counter))))

(define (get-llvm-block-id counter)
  (string-append "x" (number->string (counter))))


(define (get-value exp)
  (if (equal? (car exp) 'incomplete)
      (third exp)
      (second (last exp))))

(define (get-code exp)
  (cond [(empty? exp) '()]
        [(equal? (car exp) 'incomplete) '()]
        [else exp]))

(define (get-type exp)
  (if (equal? (car exp) 'incomplete)
      (second exp)
      (first (last exp))))
  
; loop over elem and use eval to deal 
(define (loop-elem elem-list . rest)
  (reverse (foldl
            (lambda (elem res)
              (let ([type (car elem)]
                    [content (cdr elem)])
                (cons 
                 (apply (elem-eval type) (cons content rest))
                 res)))
            '()
            elem-list)))

(define (get-llvm-type type)
  (cond [(equal? 'Int type) "i32"]
        [(equal? '() type) "i32"]
        [(equal? 'i32 type) "i32"]
        [(and (list? type)
              (equal? (car type) 'ptr))
         (string-append (get-llvm-type (cdr type)) "*")]
        [(list? type)
         (string-append "[" (number->string (car type)) " x " (get-llvm-type (cdr type)) "]")]
        [else type]))

; return block of codes
(define (CompUnit ast)
  (let* ([global-hash (make-hash)]
         [counter (make-counter)]
         [var-content
          (loop-elem
           (filter
            (lambda (x) (equal? (car x) 'GlobalDecl)) ast)
           (list global-hash)
           counter)]
         [func-content
          (loop-elem
           (filter (lambda (x) (equal? (car x) 'FuncDef)) ast)
           (list global-hash)
           counter)])
    (append
     ; library functions included
     (list
      (map
       (lambda (x)
         (list
          'declare
          (func-feat-ret (sym-feat x))
          (sym-id x)
          (add-between (map get-llvm-type (func-feat-para (sym-feat x))) ",")))
       (set->list func-include)))
     ; global varible declared
     (list (apply append var-content))
     ; functions declared
     func-content)))


(define (GlobalDecl ast symbols counter)
  ; test if is a const decl
  (define is-const (equal? (cadr ast) 'ConstDecl))
  (define type 'i32) ; TODO: should consider array later
  (define vars
    (append*
     (list (if is-const (fifth ast) (fourth ast))) ; first var 
     (map cdr (if is-const (sixth ast) (fifth ast))))) ;remaining
  (map
   (lambda (x)
     (let* ([name (token-value (second x))]
            [array-info (map (lambda (x) (cal-const(second x))) (third x))]
            [value-expr (fourth x)]
            [value
             (if (empty? value-expr)
                 (if (empty? array-info) 0 'zeroinitializer)
                 (implement-const
                  array-info
                  (cal-const (cdr (second value-expr)))))])
       (when (not (empty? array-info))
         (when (foldl
                (lambda (x y) (or x y))
                #f
                (map (lambda (x) (<= x 0)) array-info))
           (error "array size under 0")))
       (try-hash-set!
        (car symbols)
        name
        (sym name (get-global-var name)
             (if(empty? array-info) 'i32 array-info)
             (num-feat (if is-const 'const 'var))))
       (when is-const (registe-var name array-info value))
       (flatten(list (get-global-var name) '= 'dso_local 'global (get-llvm-init array-info value)))))
   vars))
  

(define (BType ast symbols counter)
  'i32)

(define (Decl ast symbols counter [block-start '()] [block-end '()])
  ((elem-eval (car ast)) (cdr ast) symbols counter))

(define (VarDecl ast symbols counter)
  (define type 'i32)
  (define vars (cons (cadr ast) (map cadr (caddr ast))))
  
  (apply append
         (map (lambda (var) (VarDef (cdr var) symbols counter type))
              vars)))

(define (VarDef ast symbols counter type)
  (define name (token-value (car ast)))
  (define array-info (map (lambda (x) (cal-const (second x)))(second ast)))
  (define id (get-llvm-var counter)) ; create a i32*
  ; put the symbol into hash
  (if (empty? array-info)
      ; if it's number
      (let ([exp (if (empty? (third ast))
                     '()
                     (InitVal (cdr (second (third ast))) symbols counter id))])
        (try-hash-set! (car symbols) name (sym name id type (num-feat 'var)))
        ; give it inital value
        (cons (list 'void id '= 'alloca 'i32)
              exp))
      ; if it's an array
      (let* ([exp (second ast)]
             [head (get-llvm-var counter)])
        (when (foldl
               (lambda (x y) (or x y))
               #f
               (map (lambda (x) (<= x 0)) array-info))
          (error "array size under 0"))
        (try-hash-set! (car symbols) name (sym name id array-info (num-feat 'var)))
        (set-add! func-include (hash-ref lib-func "memset"))
        (append* (list (cons array-info (flatten (list id '= 'alloca (get-llvm-type array-info)))))
                 (list (flatten (list 'void head '= 'bitcast (get-llvm-type array-info) '* id 'to 'i32*)))
                 (list (list 'void 'call 'void '@memset "(" 'i32* head "," 'i32 0 "," 'i32 (* 4(apply * array-info)) ")" ))
                 (if (empty? (third ast))
                     '()
                     (list (InitVal (cdr (second (third ast))) symbols counter id array-info)))))))

(define (generate-store exp pos)
  (append
   (get-code exp)
   (list (list 'void 'store 'i32 (get-value exp) "," 'i32* pos))))

(define (generate-get-ptr shape ptr pos counter)
  (define type (get-llvm-type shape))
  (define id (get-llvm-var counter))
  (define id2 (get-llvm-var counter))
  
  (if (equal? 'ptr (car shape))
      (list
       (list 'void id2 '= 'load type "," type '* ptr)
       (cons
        (if (empty? (cdr shape)) 'i32 (cdr shape))
        (flatten(list
                 id '=
                 'getelementptr  (get-llvm-type (cdr shape))
                 "," (get-llvm-type (cdr shape)) '* id2 ","
                 (list 'i32 pos)))))
      
      (list (cons
             (if (empty? (cdr shape)) 'i32 (cdr shape))
             (flatten(list
                      id '=
                      'getelementptr type "," type '* ptr ","
    
                      (list 'i32 0 "," 'i32 pos )))))))

(define (InitArr ast symbols counter pos [shape '()])
  (define content
    (if (empty? (second ast)) '() (append (list (car (second ast))) (map second (cadr (second ast))))))
  (define ptrs (map (lambda (x) (generate-get-ptr shape pos x counter))
                    (range (length content))))

  (append* (append* '() ptrs)
           (map
            (lambda (x p)
              ((elem-eval (car x)) (cdr x) symbols counter (get-value p) (cdr shape)))
            content ptrs)))
 

(define (InitVal ast symbols counter [pos '()] [shape '()])
  (define exp (if (or (equal? (car ast) 'Exp)
                      (equal? (car ast) 'ConstExp))
                  ((elem-eval (car ast)) (cdr ast) symbols counter)
                  ((elem-eval (car ast)) (cdr ast) symbols counter pos shape)))
  (if (or (equal? (car ast) 'InitArr)
          (equal? (car ast) 'ConstInitArr))
      exp
      (generate-store exp pos)))

; seems unused
(define (ConstDecl ast symbols counter)
  ;(ConstDecl . (SEQ Const BType ConstDef (REPEAT . (SEQ Comma ConstDef )) Semicolon))
  (define type 'i32)
  (define symbol-part (cddr ast))
  (define vars (cons (car symbol-part)  (map cadr (cadr symbol-part))))
  
  (apply append
         (map (lambda (var) (ConstDef (cdr var) symbols counter type))
              vars)))

(define (ConstDef ast symbols counter type)
  (VarDef ast symbols counter type))

(define (ConstInitArr ast symbols counter pos [shape '()])
  (InitArr ast symbols counter pos shape))
  
(define (ConstInitVal ast symbols counter [pos '()] [shape '()])
  (InitVal (cdr ast) symbols counter pos shape))

(define (ConstExp ast symbols counter)
  (writeln ast)
  (error 'here)
  (AddExp ast symbols counter 'const))

  
(define (FuncDef ast symbols counter)
  (let* ([counter (make-counter)]
         [ret-type (if (equal? 'Int (token-type (cdr (first ast)))) 'i32 'void)]
         [func-name (token-value (second ast))]
         [paras (if (empty? (fourth ast))
                    '()
                    (FuncFParams (cdr (fourth ast)) symbols counter))]
         [content (last ast)]
         [global-symbols (last symbols)])
    ; check if this function name has already been used.
    ; add to globol symbol table if not.
    (try-hash-set!
     global-symbols
     func-name
     (sym func-name
          (string-append "@" func-name)
          'function
          (func-feat ret-type
                     (map (lambda (x) (sym-type (cdr x))) paras))))
    ; deal with the function content
    (append (append*
             (list (list
                    'define
                    'dso_local
                    (get-llvm-type ret-type)
                    (string-append "@" func-name)
                    (flatten (add-between
                              (map (lambda (x)
                                     (list (get-llvm-type (sym-type x))
                                           (sym-id x)))
                                   (map cdr paras))
                              ","))))
             (Block (cdr content) symbols counter '() '() paras))
            (list (list 'void 'ret (if (equal? ret-type 'void) 'void "i32 0"))))))

(define (FuncFParams ast symbols counter)
  (map (lambda (x)
         (FuncFParam (cdr x) symbols counter))
       (cons (car ast) (map second (second ast)))))


(define (FuncFParam ast symbols counter)
  (define name (token-value (second ast)))
  (define array-info (third ast))
  (define type
    (if (empty? array-info)
        'i32
        (cons
         'ptr
         (map (lambda (x) (cal-const (second x))) (third array-info)))))
  (cons name (sym name (get-llvm-var counter) type (num-feat 'var))))
  
(define (Block ast symbols counter [block-start '()] [block-end '()] [args '()])
  ;TODO: need to add args to block-hash

  (let ([block-hash (make-hash)])
    (append
     (append (map (lambda (x)
                    (let ([id (get-llvm-var counter)]
                          [name (car x)]
                          [value (sym-id (cdr x))]
                          [type (sym-type (cdr x))])
                      (try-hash-set! block-hash name (sym name id type (num-feat 'var)))
                      (list
                       (list 'void id '= 'alloca (get-llvm-type type))
                       (list 'void 'store (get-llvm-type type) value "," (get-llvm-type type) '* id))))
                  args))
     (loop-elem
      (car (filter list? ast))
      (cons block-hash symbols)
      counter
      block-start
      block-end))))

(define (Stmt ast symbols counter [block-start '()] [block-end '()])
  ; Stmt -> Assign | Exp-Stmt | Block | If | While | Break | Continue | Return
  ; block-start and block-end should only be used by break and continue statmenet
  (let ([res ((elem-eval (car ast)) (cdr ast) symbols counter block-start block-end)])
    (if (equal? (car ast) 'Block)
        (append* '()  res)
        res)))

(define (Ret ast symbols counter [block-start '()] [block-end '()])
  ; Return -> 'return' [Exp] ';'
  (define ret-value (if (empty? (second ast))
                        'void
                        (Exp (cdadr ast) symbols counter)))
  (if (empty? (second ast))
      (list (list 'void 'ret 'void))
      (append (get-code ret-value)
              (list  (list
                      'void
                      'ret
               
                  
                      'i32 (get-value ret-value))))))

(define (Empty-Stmt ast symbols counter [block-start '()] [block-end '()])
  '())

(define (If-Stmt ast symbols counter [block-start '()] [block-end '()])
  (define condition
    (type-cast (Cond (cdr (third ast)) symbols counter) 'i1 counter))
  (define stmt1 (Stmt (fifth ast) symbols counter block-start block-end))
  (define stmt2
    (let ([part (sixth ast)])
      (if (empty? part)
          '()
          (Stmt (second part) symbols counter block-start block-end))))
  (if (empty? stmt2)
      (let ([block1 (get-llvm-block-id counter)]
            [block2 (get-llvm-block-id counter)])    
        (append
         (get-code condition)
         (list (list
                'void
                'br 'i1
                (get-value condition) ","
                'label (string-append "%" block1) ","
                'label (string-append "%" block2)))
         (list (list 'label (string-append block1 ":")))
         (get-code stmt1)
         (list (list 'void 'br 'label (string-append "%" block2)))
         (list (list 'label (string-append block2 ":")))))
      (let ([block1 (get-llvm-block-id counter)]
            [block2 (get-llvm-block-id counter)]
            [block3 (get-llvm-block-id counter)])
        (append
         (get-code condition)
         (list (list
                'void
                'br 'i1
                (get-value condition) ","
                "label" (string-append "%" block1) ","
                "label" (string-append "%" block2)))
         (list (list 'label (string-append block1 ":")))
         (get-code stmt1)
         (list (list 'void 'br 'label (string-append "%" block3))) ; jump to end
         (list (list 'label (string-append block2 ":")))
         (get-code stmt2)
         (list (list 'void 'br 'label (string-append "%" block3))) ;jump to end
         (list (list 'label (string-append block3 ":")))))))

(define (Expr-Stmt ast symbols counter [block-start '()] [block-end '()])
  (Exp (cdar ast)  symbols counter))

(define (Assign-Stmt ast symbols counter [block-start '()] [block-end '()])
  ;(Assign-Stmt . (SEQ LVal Assign Exp Semicolon))
  (define ori-lval (LVal (cdar ast) symbols counter))
  (define lval  (LVal (cdar ast) symbols counter)) 
  (when (not (or (equal? (get-type lval) 'i32)
                 (equal? (get-type lval) '())))
    (error "can' assign"))
  (when (equal? 'const (get-type ori-lval))
    (error "constant is not a legal left value"))
  (when (equal? 'function (get-type ori-lval))
    (error "function is not a legal left value"))
  (define exp (Exp (cdr(list-ref ast 2)) symbols counter))
  (append
   (get-code lval)
   (get-code exp)
   (list (list 'i32 "store" 'i32 (get-value exp) ", i32*" (get-value lval)))))

(define (While-Stmt ast symbols counter [block-start '()] [block-end '()])
  ; actually here is the only source of block-start and block-end
  (define cond-start (get-llvm-block-id counter))
  (define content-start (get-llvm-block-id counter))
  (define end (get-llvm-block-id counter))

  (define condition (Cond (cdr (third ast)) symbols counter))
  (define content
    (Stmt (cdr (fifth ast)) symbols counter (string-append "%" cond-start) (string-append "%" end)))
  (append
   ; jump to start of while (in case there is a unused block id)
   (list (list 'void 'br 'label (string-append "%" cond-start)))
   
   (list (list 'label (string-append cond-start ":")))
   (get-code condition)
   (list (list
          'void
          'br 'i1
          (get-value condition) ","
          'label (string-append "%" content-start) ","
          'label (string-append "%" end)))

   (list (list 'label (string-append content-start ":")))
   (get-code content)
   (list (list 'void 'br 'label (string-append "%" cond-start))) ;jump to end
   (list (list 'label (string-append end ":")))))

(define (Break-Stmt ast symbols counter [block-start '()] [block-end '()])
  (list (list 'void 'br 'label block-end)))

(define (Cont-Stmt ast symbols counter [block-start '()] [block-end '()])
  (list (list 'void 'br 'label block-start)))



(define (LVal ast symbols counter [mode 'val])  
  (define name (token-value (car ast)))
  (define array-par (map
                     (lambda (x)
                       ((elem-eval (car (second x))) (cdr (second x)) symbols counter))
                     (second ast)))
  (define prev-code (map get-code array-par))
  (define array-info (map get-value array-par))

  (let iter ([symbol-list symbols])
    ; if can't find the symbol
    (if (empty? symbol-list)
        (error "symbol not declared")
        (let ([cur-hash (car symbol-list)])
          ; find in current-hash
          (if (hash-has-key? cur-hash name)
              (let ([symbol (hash-ref cur-hash name)])
                (when (and (equal? mode 'const)
                           (not (equal? 'const (num-feat-const (sym-feat symbol)))))
                  (error "expect a const"))
                (if (pair? array-info)
                    (append (append* '() prev-code)
                            (let loop ([shape (sym-type symbol)]
                                       [ref array-info]
                                       [ptr (sym-id symbol)])
                              (if (empty? ref)
                                  '()
                                  (let ([prev (generate-get-ptr shape ptr (car ref) counter)])
                                    (append
                                     prev
                                     (loop (cdr shape) (cdr ref) (get-value prev)))))))
                    (list 'incomplete
                          (if (equal? 'const (num-feat-const (sym-feat symbol))) 'const (sym-type symbol))
                          (sym-id symbol))))
              (iter (cdr symbol-list)))))))
              

(define (Exp ast symbols counter)
  ; Exp -> AddExp
  (AddExp (cdr ast) symbols counter))

(define (type-cast exp type counter)
  (define ori-type (get-type exp))
  (define value (get-value exp))
  (cond [(equal? ori-type type) exp]
        [(and (equal? ori-type 'i32)
              (equal? type 'i1))
         (append
          (get-code exp)
          (list (list
                 'i1
                 (get-llvm-var counter)
                 '= 'icmp 'ne
                 'i32 (get-value exp) ","
                 0)))]
        [(and (equal? ori-type 'i1)
              (equal? type 'i32))
         (append
          (get-code exp)
          (list (list
                 'i32
                 (get-llvm-var counter)
                 '= 'zext
                 'i1 (get-value exp)
                 'to 'i32)))]))

(define (generate-ir-expr op num1 num2 counter)
  ; to generate ir code for expressions like : 1 + 2
  (define type-info (hash-ref op-hash op))
  
  (define op-ir (operation-ir type-info))
  (define type-needed (operation-operand-type type-info))
  (define ret-type (operation-ret-type type-info))
  
  (define cast-num1 (type-cast num1 type-needed counter))
  (define cast-num2 (type-cast num2 type-needed counter))
  (append
   (get-code cast-num1)
   (get-code cast-num2)
   (list (flatten (list
                   ret-type
                   (get-llvm-var counter)
                   '=
                   op-ir type-needed
                   (get-value cast-num1) ","
                   (get-value cast-num2))))))
  
(define (cal-seq-exp ast symbols counter [mode 'val])
  ;this function is for AddExp and MulExp, for they have identify form
  (writeln (car ast))
  (let add-loop ([loop-list (cadr ast)]
                 ; first operand, must exist
                 [add1 ((elem-eval (caar ast)) (cdar ast) symbols counter mode)])
    ; caculate the remaining part, loop over the remaining part
    (if (empty? loop-list)
        add1
        (let* ([item (car loop-list)]
               [op (token-type (car item))]
               [add2  ((elem-eval (caadr item)) (cdadr item) symbols counter mode)]
               [remaining (cdr loop-list)])
          (add-loop remaining (generate-ir-expr op add1 add2 counter))))))

(define (AddExp ast symbols counter [mode 'val])
  ; AddExp -> MulExp { ('+' | '-') MulExp }
  (cal-seq-exp ast symbols counter mode))
  
(define (MulExp ast symbols counter [mode 'val])
  ; MulExp -> UnaryExp { ('*' | '/' | '%') UnaryExp }
  (cal-seq-exp ast symbols counter mode))

(define (Cond ast symbols counter)
  (type-cast (LOrExp (cdr ast) symbols counter) 'i1 counter))

(define (LOrExp ast symbols counter [mode 'val])
  ; LOrExp -> LAndExp { '||' LAndExp }
  (cal-seq-exp ast symbols counter mode))

(define (LAndExp ast symbols counter [mode 'val])
  ; LAndExp -> EqExp { '&&' EqExp }
  (cal-seq-exp ast symbols counter mode))

(define (EqExp ast symbols counter [mode 'val])
  (cal-seq-exp ast symbols counter mode))

(define (RelExp ast symbols counter [mode 'val])
  (cal-seq-exp ast symbols counter mode))

(define (UnaryExp ast symbols counter [mode 'val])
  (cond   
    [(empty? ast) '()]
    ; if is PrimaryExp
    [(equal? 'FuncCall (car ast))
     (FuncCall (cdr ast) symbols counter)]
    [(equal? 'PrimaryExp (car ast))
     (PrimaryExp (cdr ast) symbols counter mode)]
    ; UnaryOp UnaryExp
    [(equal? 'UnaryOp (caar ast))
     (let ([op (token-type (cdar ast))]
           [exp (UnaryExp (cdadr ast) symbols counter mode)])
       (cond
         [(equal? op 'Plus) exp] 
         [(equal? op 'Minus)
          (generate-ir-expr op (Number (token 'Number 0)) exp counter)]
         [(equal? op 'Not)
          (generate-ir-expr 
           'Equal (list 'incomplete (get-type exp) 0) exp counter)]))]))
    

(define (PrimaryExp ast symbols counter [mode 'val])
  (cond
    ; if is just a number
    [(struct? ast) (Number ast)]
    ; if is a lVal
    [(equal? (car ast) 'LVal)
     (let* ([value (LVal (cdr ast) symbols counter mode)]
            ; [value-ptr (get-code-and-num value)]
            [prev-code (get-code value)]
            [id (get-value value)])
       (if (list? (get-type value))
           (append
            prev-code
            (generate-get-ptr (get-type value) (get-value value) 0 counter))
           (append
            prev-code
            (list (list
                   'i32
                   (get-llvm-var counter)
                   '= "load" "i32, i32*"
                   id)))))]
    ; if is '(' Exp ')'
    [(equal? (token-type (car ast)) 'LPar)
     (Exp (cdadr ast) symbols counter)]
    [else '()]))


(define (FuncCall ast symbols counter)
  (define name (token-value (car ast)))
  ;define act-para
  (define paras
    (if (empty? (third ast))
        '()
        (let ([para-list (cdr(third ast))])
          (map (lambda (x)
                 (Exp (cdr x) symbols counter))
               (cons (car para-list)
                     (map cadr (cadr para-list)))))))
  ; get func-def
  (define func-def
    (cond
      [(hash-has-key? (last symbols) name)
       (hash-ref (last symbols) name)]
      [(hash-has-key? lib-func name)
       (set-add! func-include (hash-ref lib-func name))
       (hash-ref lib-func name)]
      [else
       (error (string-append name ": function not declared"))]))
  (define ret-type (func-feat-ret (sym-feat func-def)))
  (when (not (equal? (length (func-feat-para (sym-feat func-def))) (length  paras)))
    (error (string-append name ":function call with wrong parameters")))
  
  (append
   (foldl append '() (map get-code paras))
   (list (cons ret-type (append
                         (if (equal? 'void (func-feat-ret (sym-feat func-def)))
                             '()
                             (list (get-llvm-var counter) '=))
                         (list
                          'call
                          (get-llvm-type (func-feat-ret (sym-feat func-def)))
                          (sym-id func-def)
                          (flatten
                           (add-between
                            (map
                             (lambda (x y)
                               (cons (get-llvm-type x) y))
                             (func-feat-para
                              (sym-feat func-def))
                             (map get-value paras)) ","))))))))

(define (Number token)
  (list 'incomplete 'i32 (token-value token)))

(define ast (parser))
(define (ir-list-generator [ast ast])
  (CompUnit (cdar ast)))

(ir-list-generator (parser))