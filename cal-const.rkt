#lang racket
(require "./lexer.rkt")
(provide cal-const)
(provide registe-var)

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (elem-eval elem)
  (eval elem ns))


(struct global-sym (type val) #:transparent)
(define global-var-hash (make-hash))

(define (registe-var name type val)
  (hash-set! global-var-hash name (global-sym type val)))

(define (cal-const ast)
  (Exp (cdr ast)))

(define (Exp ast)
  (AddExp (cdr ast)))

(define (cal-seq-exp ast)
  ;this function is for AddExp and MulExp, for they have identify form
  (let add-loop ([loop-list (cadr ast)]
                 ; first operand, must exist
                 [add1 ((elem-eval (caar ast)) (cdar ast))])
    ; caculate the remaining part, loop over the remaining part
    (if (empty? loop-list)
        add1
        (let* ([item (car loop-list)]
               [op (token-type (car item))]
               [add2  ((elem-eval (caadr item)) (cdadr item))]
               [remaining (cdr loop-list)])
          (add-loop remaining (cal op add1 add2))))))

(define (cal op num1 num2)
  ; to generate ir code for expressions like : 1 + 2
  (define op-racket
    (cond [(equal? 'Plus op) +]
          [(equal? 'Minus op) -]
          [(equal? 'Mult op) *]
          [(equal? 'Div op) quotient]
          [(equal? 'Mod op) remainder]))
  (op-racket num1 num2))

(define (AddExp ast)
  (cal-seq-exp ast))

(define (MulExp ast)
  (cal-seq-exp ast))

(define (UnaryExp ast)
  (cond
    [(equal? 'FuncCall (car ast))
     (error "can't get result with function in expression")]
    ; if is PrimaryExp
    [(equal? 'PrimaryExp (car ast))
     (PrimaryExp (cdr ast))]
    ; UnaryOp UnaryExp
    [(equal? 'UnaryOp (caar ast))
     (let ([op (token-type (cdar ast))]
           [exp (UnaryExp (cdadr ast))])
       (cond
         [(equal? op 'Plus) exp] 
         [(equal? op 'Minus) (- exp)]
         [(equal? op 'Not) (if (equal? 0 exp) 1 0)]))]))

(define (PrimaryExp ast)
  (cond
    ; if is just a number
    [(struct? ast) (token-value ast)]
    ; if is a lVal
    [(equal? (car ast) 'LVal) (LVal (cdr ast))]
    ; if is '(' Exp ')'
    [(equal? (token-type (car ast)) 'LPar)
     (Exp (cdadr ast))]))

(define (LVal ast)  
  (define name (token-value (car ast)))
  (if (hash-has-key? global-var-hash name)
      (global-sym-val (hash-ref global-var-hash name))
      (error "symbol not defined")))