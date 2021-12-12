#lang racket
(require "./lexer.rkt")
(provide cal-const)
(provide registe-var)
(provide implement-const)
(provide get-llvm-type)
(provide get-llvm-init)

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (elem-eval elem)
  (eval elem ns))


(struct global-sym (type val) #:transparent)
(define global-var-hash (make-hash))

(define (registe-var name type val)
  (hash-set! global-var-hash name (global-sym type val)))

;(define (format-global-init type arr)

(define (cal-const ast)
  ;(writeln ((elem-eval (car ast)) (cdr ast)))
  ((elem-eval (car ast)) (cdr ast)))

(define (implement-const shape value)
  (define (iter shape value)
    (if (empty? shape)
        (if (empty? value) 0 value)
        (let loop ([len (car shape)]
                   [child-shape (cdr shape)]
                   [init value])
          (if (equal? 0 len)
              '()
              (cons (iter child-shape (if (empty? init) '() (car init)))
                    (loop (- len 1) child-shape (if (empty? init) '() (cdr init))))))))
  (iter shape value))
               
(define (get-llvm-type type)
  (if (empty? type)
      'i32
      (flatten (list "[" (car type) 'x  (get-llvm-type (cdr type)) "]"))))

(define (get-llvm-init shape value)
  (flatten (list (get-llvm-type shape)
                 (if (or
                      (empty? shape)
                      (not (pair? value)))
                     value
                     (list "[" (add-between (map
                                             (lambda (x) (get-llvm-init (cdr shape) x))
                                             value) ",") "]")))))


(define (cal-array ast)
  (define array (second ast))
  (if (empty? array)
      '()
      (map (lambda (x) ((elem-eval (cadr x)) (cddr x)))
           (append* (list (first array)) (map cdr (second array))))))


(define (ConstInitArr ast)
  (cal-array ast))

(define (InitArr ast)
  (cal-array ast))

(define (ConstExp ast)
  ((elem-eval (car ast)) (cdr ast)))

(define (ConstInitVal ast)
  (cal-array ast))

(define (Exp ast)
  ((elem-eval (car ast)) (cdr ast)))

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
  (define pos (map (lambda (x) (cal-const (second x))) (second ast)))
  (define (ref pos value)
    (if (empty? pos)
        value
        (ref (cdr pos) (list-ref value (car pos)))))
  (if (hash-has-key? global-var-hash name)
      (ref pos (global-sym-val (hash-ref global-var-hash name)))
      (error "symbol not defined")))