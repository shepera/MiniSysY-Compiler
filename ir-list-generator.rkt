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

(define (AddExp ast symbols counter)
  (MulExp (cdar ast) symbols counter)) ;TODO
  
(define (MulExp ast symbols counter)
  (UnaryExp (cdar ast) symbols counter)) ;TODO

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
          (let ([prev (get-code-and-num exp)])
            (append
             (car prev)
             (list (list
                    (string-append
                     "%" (number->string (counter)))
                    " = sub i32 0, "
                    (cdr prev)))))]))]))
    

(define (PrimaryExp ast symbols counter)
  (cond
    ; if is just a number
    [(struct? ast) (Number ast)]
    ; if is '(' Exp ')'
    [(equal? (token-type (car ast)) 'LPar) (Exp  (cdadr ast) symbols counter)]
    [else '()]))
  


  
(define (Number token)
  (list 'incomplete 'i32 (token-value token)))
        
(define (ir-list-generator [ast ast])
  (CompUnit (cdar ast)))


(ir-list-generator)