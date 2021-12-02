#lang racket

(require "./parser.rkt")
(require "./lexer.rkt")
(provide ir-list-generator)

; get namespace for eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (elem-eval elem)
  (eval elem ns))

(struct symbol (name id type)  #:transparent)

; a counter maker, will increase by itself
(define (make-counter)
  (define a 0)
  (lambda () (set! a (+ 1 a)) a))

; try to add a key-value pair in hash,
; error when failed
(define (try-hash-set! hash key value)
  (if (hash-has-key? hash key)
      (error "duplicate key")
      (hash-set!
       hash
       key
       value)))

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
            (lambda ( elem res)
              (let ([elem (car elem-list)])
                (let ([type (car elem)]
                      [content (cdr elem)])
                  (cons 
                   ((elem-eval type) content hash counter)
                   res))))
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
     (symbol func-name (string-append "@" func-name) 'function))
    (cons
     (list
      'define
      'dso_local
      (get-llvm-type ret-type)
      (string-append "@" func-name)
      "()")
     (car (Block (cdr content) symbols counter)))))
  
(define (Block ast symbols counter [args '()])
  ;TODO: need to add args to block-hash
  (let ([block-hash (make-hash)]) 
    (loop-elem
     (filter list? ast)
     (cons block-hash symbols)
     counter)))


(define (Stmt ast symbols counter)
  ((elem-eval (car ast)) (cdr ast) symbols counter))

(define (Ret ast symbols counter)
  (define ret-value (get-code-and-num (Exp (cdadr ast) symbols counter)))
  (append (car ret-value)
          (list(list
                'ret
                'i32 (cdr ret-value)))))



(define (Exp ast symbols counter)
  (AddExp (cdr ast)  symbols counter))

(define (generate-ir-expr op num1 num2 counter)
  (define op-ir
    (cond [(equal? op 'Plus) 'add]
          [(equal? op 'Minus) 'sub]
          [(equal? op 'Mult)  'mul]
          [(equal? op 'Div) 'sdiv]
          [(equal? op 'Mod ) 'srem]))
  (let ([num1-ir (get-code-and-num num1)]
        [num2-ir (get-code-and-num num2)])
    (writeln num1-ir)
    (writeln num2-ir)
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
  
(define (cal-seq-exp ast symbols counter)
  (let add-loop ([loop-list (cadr ast)]
                 [add1 ((elem-eval (caar ast)) (cdar ast) symbols counter)] )
    (if (empty? loop-list)
        add1
        (let* ([item (car loop-list)]
               [op (token-type (car item))]
               [add2  ((elem-eval (caadr item)) (cdadr item) symbols counter)]
               [remaining (cdr loop-list)])
          (add-loop remaining (generate-ir-expr op add1 add2 counter))))))

(define (AddExp ast symbols counter)
  ; and caculate with the remaining part
  (cal-seq-exp ast symbols counter))
  
(define (MulExp ast symbols counter)
    (cal-seq-exp ast symbols counter))

(define (UnaryExp ast symbols counter)
  (cond   
    [(empty? ast) '()]
    ; if is PrimaryExp
    [(equal? 'PrimaryExp (car ast))
     (PrimaryExp (cdr ast) symbols counter)]
    ; UnaryOp UnaryExp
    [(equal? 'UnaryOp (caar ast))
     (let ([op (token-type (cdar ast))]
           [exp (UnaryExp (cdadr ast) symbols counter)]);;;;;
       (cond
         [(equal? op 'Plus) exp] 
         [(equal? op 'Minus)
          (generate-ir-expr op (Number (token 'Number 0)) exp counter)
          ]))]))
    

(define (PrimaryExp ast symbols counter)
  (cond
    ; if is just a number
    [(struct? ast) (Number ast)]
    ; if is '(' Exp ')'
    [(equal? (token-type (car ast)) 'LPar) (Exp (cdadr ast) symbols counter)]
    [else '()]))
  


  
(define (Number token)
  (list 'incomplete 'i32 (token-value token)))
        
(define (ir-list-generator [ast ast])
  (CompUnit (cdar ast)))


;(ir-list-generator)