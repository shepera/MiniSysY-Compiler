#lang racket

(require "./parser.rkt")
(require "./lexer.rkt")

; get namespace for eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (elem-eval elem)
  (eval elem ns))

(struct symbol (name id type)  #:transparent)

; a counter maker, will increase by itself
(define (make-counter)
  (define a -1)
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
     (symbol func-name func-name 'function))
    (cons
     (list
      'define
      'dso_local
      (get-llvm-type ret-type)
      (string-append "@" func-name)
      "()")
     (Block (cdr content) symbols counter))))
  
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
  (define ret-value (list-ref ast 1))
  (cons 'ret (Number ret-value)))

(define (Number token)
  (list 'i32 (token-value token)))
        
(define (ir-list-generator [ast ast])
  (CompUnit (cdar ast)))
(display (ir-list-generator))