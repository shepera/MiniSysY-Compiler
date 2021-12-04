#lang racket

(require "./parser.rkt")
(require "./lexer.rkt")
(require "./tools.rkt")
(provide ir-list-generator)

; get namespace for eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (elem-eval elem)
  (eval elem ns))

(struct symbol (name id type feat)  #:transparent)
(struct num-feat (const))

; return (prev-code . value))
(define (get-code-and-num exp)
  (if (equal? (car exp) 'incomplete)
      (cons '() (caddr exp))
      (cons
       exp
       (car (list-ref exp (- (length exp) 1))))))
  
; loop over elem and use eval to deal 
(define (loop-elem elem-list hash counter)
  (reverse (foldl
            (lambda (elem res)
              (let ([type (car elem)]
                    [content (cdr elem)])
                (cons 
                 ((elem-eval type) content hash counter)
                 res) ))
            '()
            elem-list)))

(define (get-llvm-type type)
  (cond [(equal? 'Int type) 'i32]))
  
(define ast (parser))


; return block of codes
(define (CompUnit ast)
  (let ([global-hash (make-hash)]
        [counter (make-counter)])
    (loop-elem ast (list global-hash) counter)))

(define (BType ast symbols counter)
  'i32)

(define (Decl ast symbols counter)
  ((elem-eval (car ast)) (cdr ast) symbols counter))

(define (VarDecl ast symbols counter)
  (define type 'i32)
  (define vars (cons (cadr ast) (map cadr (caddr ast))))  
  (apply append
         (map (lambda (var) (VarDef (cdr var) symbols counter type))
              vars)))

(define (VarDef ast symbols counter type)
  (define id (string-append "%" (number->string (counter)))) ; create a i32*
  (define init-val
    (if (empty? (cadr ast))
        '()
        (get-code-and-num (InitVal (cdr (cadadr ast)) symbols counter))))
  (define name (token-value (car ast)))
  ; put the symbol into hash
  (try-hash-set! (car symbols) name (symbol name id type (num-feat 'var)))
  ; give it inital value
  (cons (list id "= alloca i32")
        (if (empty? init-val)
            '()
            (append
             (car init-val)
             (list (list "store i32" (cdr init-val) ", i32*" id))))))
  

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
  (define id (string-append "%" (number->string (counter)))) ; create a i32*
  (define init-val
    (get-code-and-num (ConstInitVal (cdr (cdaddr ast)) symbols counter)))
  (define name (token-value (car ast)))
  ; put the symbol into hash
  (try-hash-set! (car symbols) name (symbol name id type (num-feat 'const)))
  ; give it inital value
  
  (cons (list id "= alloca i32")
        (append
         (car init-val)
         (list (list "store i32" (cdr init-val) ", i32*" id)))))

(define (ConstInitVal ast symbols counter)
  
  (ConstExp (cdr ast) symbols counter))

(define (ConstExp ast symbols counter)
  ; (write ast)
  (AddExp ast symbols counter 'const))

  
(define (FuncDef ast symbols counter)
  (let ([ret-type (token-type (cdr(list-ref ast 0)))]
        [func-name  (token-value (list-ref ast 1))]
        [content (list-ref ast (- (length ast) 1))]
        [global-symbols (list-ref symbols 0)]
        [counter (make-counter)])
    ; check if this function name has already been used.
    ; add to globol symbol table if not.
    (try-hash-set!
     global-symbols
     func-name
     (symbol func-name (string-append "@" func-name) 'function '()))
    ; deal with the function content
    (append*
     (list(list
           'define
           'dso_local
           (get-llvm-type ret-type)
           (string-append "@" func-name)
           "()"))
     (Block (cdr content) symbols counter))))
  
(define (Block ast symbols counter [args '()])
  ;TODO: need to add args to block-hash
  (let ([block-hash (make-hash)])
    (loop-elem
     (car (filter list? ast))
     (cons block-hash symbols)
     counter)))


(define (Stmt ast symbols counter)
  ;Stmt -> Assign | Exp-Stmt | Block | If | While | Break | Continue | Return
  ((elem-eval (car ast)) (cdr ast) symbols counter))

(define (Ret ast symbols counter)
  ; Return -> 'return' [Exp] ';'
  (define ret-value (get-code-and-num (Exp (cdadr ast) symbols counter)))
  (append (car ret-value)
          (list(list
                'ret
                'i32 (cdr ret-value)))))


(define (Empty-Stmt ast symbols counter)
  '())

(define (Expr-Stmt ast symbols counter)
  '())

(define (Assign-Stmt ast symbols counter)
  ;(Assign-Stmt . (SEQ LVal Assign Exp Semicolon))
  (define ori-lval (LVal (cdar ast) symbols counter))
  (define lval (get-code-and-num (LVal (cdar ast) symbols counter)))
  (when (equal? 'const (list-ref ori-lval 1))
    (error "constant is not a legal left value"))
  (when (equal? 'function (list-ref ori-lval 1))
    (error "function is not a legal left value"))
  (define exp (get-code-and-num (Exp (cdr(list-ref ast 2)) symbols counter)))
  (append
   (car lval)
   (car exp)
   (list (list "store i32" (cdr exp) ", i32*" (cdr lval)))))

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
                           (not (equal? 'const (num-feat-const (symbol-feat symbol)))))
                      (error "expect a const"))   
                (list 'incomplete
                      (if (equal? 'const (num-feat-const (symbol-feat symbol))) 'const 'i32*)
                      (symbol-id symbol)))
              (iter (cdr symbol-list)))))))
              

(define (Exp ast symbols counter)
  ; Exp -> AddExp
  (AddExp (cdr ast) symbols counter))

(define (generate-ir-expr op num1 num2 counter)
  ; to generate ir code for expressions like : 1 + 2
  (define op-ir
    (cond [(equal? op 'Plus) 'add]
          [(equal? op 'Minus) 'sub]
          [(equal? op 'Mult)  'mul]
          [(equal? op 'Div) 'sdiv]
          [(equal? op 'Mod ) 'srem]))
  (let ([num1-ir (get-code-and-num num1)]
        [num2-ir (get-code-and-num num2)])
    (append
     (car num1-ir)
     (car num2-ir)
     (list (list
            (string-append
             "%" (number->string (counter)))
            "="
            op-ir 'i32
            (cdr num1-ir)
            ","
            (cdr num2-ir))))))
  
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

(define (UnaryExp ast symbols counter [mode 'val])
  (cond   
    [(empty? ast) '()]
    ; if is PrimaryExp
    [(equal? 'PrimaryExp (car ast))
     (PrimaryExp (cdr ast) symbols counter mode)]
    ; UnaryOp UnaryExp
    [(equal? 'UnaryOp (caar ast))
     (let ([op (token-type (cdar ast))]
           [exp (UnaryExp (cdadr ast) symbols counter mode)]);;;;;
       (cond
         [(equal? op 'Plus) exp] 
         [(equal? op 'Minus)
          (generate-ir-expr op (Number (token 'Number 0)) exp counter)]))]))
    

(define (PrimaryExp ast symbols counter [mode 'val])
  (cond
    ; if is just a number
    [(struct? ast) (Number ast)]
    ; if is a lVal
    [(equal? (car ast) 'LVal)
     (let* ([value (LVal (cdr ast) symbols counter mode)]
            [value-ptr (get-code-and-num value)]
            [prev-code (car value-ptr)]
            [id (cdr value-ptr)])
       (append
        prev-code
        (list (list
               (string-append "%" (number->string (counter)))
               "=" "load" "i32, i32*"
               id)) ))]
    ; if is '(' Exp ')'
    [(equal? (token-type (car ast)) 'LPar)
     (Exp (cdadr ast) symbols counter)]
    [else '()]))


(define (Number token)
  (list 'incomplete 'i32 (token-value token)))
        
(define (ir-list-generator [ast ast])
  (CompUnit (cdar ast)))


(ir-list-generator)