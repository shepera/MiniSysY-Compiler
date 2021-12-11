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
                        "putch"  (sym "putch" "@putch" 'function (func-feat 'void '(i32)))))
                      
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
  (cond [(equal? 'Int type) 'i32]))

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
     ; function included
     (list
      (map
       (lambda (x)
         (list
          'declare
          (func-feat-ret (sym-feat x))
          (sym-id x)
          (func-feat-para (sym-feat x)))
         )
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
             (if is-const
                 (implement-const array-info (cal-const (cdr (fifth x)))) 
                 (if (empty? value-expr)
                     (if (empty? array-info) 0 'zeroinitializer)
                     (implement-const
                      array-info
                      (cal-const (cdr (second value-expr))))))])
       (try-hash-set!
        (car symbols)
        name
        (sym name (get-global-var name) 'i32 (num-feat (if is-const 'const 'var))))
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
  (define id (get-llvm-var counter)) ; create a i32*
  (define exp
    (if (empty? (cadr ast))
        '()
        (InitVal (cdr (cadadr ast)) symbols counter)))
  (define name (token-value (car ast)))
  ; put the symbol into hash
  (try-hash-set! (car symbols) name (sym name id type (num-feat 'var)))
  ; give it inital value
  (cons (list 'void id "= alloca i32")
        (if (empty? exp)
            '()
            (append
             (get-code exp)
             (list (list 'void "store" 'i32 (get-value exp) ", i32*" id))))))
  

(define (InitVal ast symbols counter)
  (Exp (cdr ast) symbols counter))

(define (ConstDecl ast symbols counter)
  ;(ConstDecl . (SEQ Const BType ConstDef (REPEAT . (SEQ Comma ConstDef )) Semicolon))
  (define type 'i32)
  (define symbol-part (cddr ast))
  (define vars (cons (car symbol-part)  (map cadr (cadr symbol-part))))
  (apply append
         (map (lambda (var) (ConstDef (cdr var) symbols counter type))
              vars)))

(define (ConstDef ast symbols counter type)
  ; TODO: changed 
  (define id (get-llvm-var counter)) ; create a i32*
  (define init-val
    (ConstInitVal (cdr (cdaddr ast)) symbols counter)) ;probably here
  (define name (token-value (car ast)))
  ; put the symbol into hash
  (try-hash-set! (car symbols) name (sym name id type (num-feat 'const)))
  ; give it inital value
  
  (cons (list 'void id "= alloca i32")
        (append
         (get-code init-val)
         (list (list 'i32 "store" 'i32 (get-value init-val) ", i32*" id)))))

(define (ConstInitVal ast symbols counter)
  (ConstExp (cdr ast) symbols counter))

(define (ConstExp ast symbols counter)
  (AddExp ast symbols counter 'const))

  
(define (FuncDef ast symbols counter)
  (let ([ret-type (token-type (cdr(list-ref ast 0)))]
        [func-name  (token-value (list-ref ast 1))]
        [content (last ast)]
        [global-symbols (list-ref symbols 0)]
        [counter (make-counter)])
    ; check if this function name has already been used.
    ; add to globol symbol table if not.
    (try-hash-set!
     global-symbols
     func-name
     (sym func-name (string-append "@" func-name) 'function '()))
    ; deal with the function content
    (append*
     (list(list
           'define
           'dso_local
           (get-llvm-type ret-type)
           (string-append "@" func-name)
           "()"))
     (Block (cdr content) symbols counter))))
  
(define (Block ast symbols counter [block-start '()] [block-end '()] [args '()])
  ;TODO: need to add args to block-hash
  (let ([block-hash (make-hash)])
    (loop-elem
     (car (filter list? ast))
     (cons block-hash symbols)
     counter
     block-start
     block-end)))

(define (Stmt ast symbols counter [block-start '()] [block-end '()])
  ; Stmt -> Assign | Exp-Stmt | Block | If | While | Break | Continue | Return
  ; block-start and block-end should only be used by break and continue statmenet
  (let ([res ((elem-eval (car ast)) (cdr ast) symbols counter block-start block-end)])
    (if (equal? (car ast) 'Block)
        (append* '()  res)
        res)))

(define (Ret ast symbols counter [block-start '()] [block-end '()])
  ; Return -> 'return' [Exp] ';'
  (define ret-value  (Exp (cdadr ast) symbols counter))
  (append (get-code ret-value)
          (list(list
                'void
                'ret
                'i32 (get-value ret-value)))))

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
  (when (equal? 'const (list-ref ori-lval 1))
    (error "constant is not a legal left value"))
  (when (equal? 'function (list-ref ori-lval 1))
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
                (list 'incomplete
                      (if (equal? 'const (num-feat-const (sym-feat symbol))) 'const 'i32*)
                      (sym-id symbol)))
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
  (LOrExp (cdr ast) symbols counter))

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
       (append
        prev-code
        (list (list
               'i32
               (get-llvm-var counter)
               '= "load" "i32, i32*"
               id)) ))]
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
      [(hash-has-key? lib-func name)
       (set-add! func-include (hash-ref lib-func name))
       (hash-ref lib-func name)]
      [else (error "function not declared")]))
  (define ret-type (func-feat-ret (sym-feat func-def)))
  (when (not (equal? (length (func-feat-para (sym-feat func-def))) (length  paras)))
    (error "function call with wrong parameters"))
  
  (append
   (foldl append '() (map get-code paras))
   (list (cons ret-type (append
                         (if (equal? 'void (func-feat-ret (sym-feat func-def)))
                             '()
                             (list (get-llvm-var counter) '=))
                         (list
                          'call
                          (func-feat-ret (sym-feat func-def))
                          (sym-id func-def)
                          (flatten
                           (add-between
                            (map
                             cons
                             (func-feat-para
                              (sym-feat func-def))
                             (map get-value paras)) ","))))))))

(define (Number token)
  (list 'incomplete 'i32 (token-value token)))

(define ast (parser))
(define (ir-list-generator [ast ast])
  (CompUnit (cdar ast)))

(ir-list-generator (parser))