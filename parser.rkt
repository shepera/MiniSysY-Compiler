#lang racket

(require "./lexer.rkt")

(provide parser)

(define tokens (lexer))
(define grammer (call-with-input-file "./grammer/grammer"
                  (lambda (in) (read in))))


; AST ( E . list( sub-AST ))
; Example:
;    E
; |--|--|
; a  F  c
;    |
;    b
; (E . (token (F . token) token)


; pattern should be a symbol / list
; return (AST . remaining tokens) / #f
(define (parser-recur [pattern 'CompUnit] [tokens tokens])
  ; try to match pattern like: A -> BC
  (define (seq-match pattern tokens)
    (if (empty? pattern)
        ; get all matched
        (cons '() tokens)
        ; get remaining pattern in the lexer
        (let ([ret (parser-recur (car pattern) tokens)])
          (if ret
              (let ([remaining-ret (seq-match (cdr pattern) (cdr ret))])
                (if remaining-ret
                    (cons (cons (car ret) (car remaining-ret))
                          (cdr remaining-ret))
                    #f))
              #f))))
  
  ; try to match pattern like: A -> B | C
  (define (alt-match pattern tokens)
    (if (empty? pattern)
        #f ; couldn't match any one
        (let ([ret (parser-recur (car pattern) tokens)])
          (if ret
              ret
              (alt-match (cdr pattern) tokens)))))
  
  ; try to match pattern like: A -> {B}
  (define (repeat-match pattern tokens)
    (if (empty? pattern)
        ; matched this repeat pattern
        (cons '() tokens)
        ; try to remaining pattern in the lexer
        (let ([ret (parser-recur pattern tokens)])
          (if ret
              ; if matched, try to match one more
              (let* ([matched (car ret)]
                     [tokens (cdr ret)]
                     [one-more (repeat-match pattern tokens)])
                (cons
                 (cons matched (car one-more))
                 (cdr one-more)))
              (cons '() tokens)))))
    
  ; try to match pattern like: A -> [B]
  (define (opt-match pattern tokens)
    (if (empty? pattern)
        ; matched this option pattern
        (cons '() tokens)
        ; try to remaining pattern in the lexer
        (let ([ret (parser-recur pattern tokens)])
          (if ret
              ret
              (cons '() tokens)))))
    
  (if (symbol? pattern)
      ; pattern is a single symbol
      (cond
        ; tokens end early
        [(empty? tokens) #f]
        ; if pattern is match with token (terminal)
        [(equal? pattern (token-type (car tokens)))
         (cons (car tokens) (cdr tokens))]
        ; got a non-terminal one
        [(hash-has-key? grammer pattern)
         (let ([ret (parser-recur (hash-ref grammer pattern) tokens)])
           (if ret
               (cons (cons pattern (car ret))
                     (cdr ret))
               #f))]
        ; get a terminal one, but not match with (car tokens)
        [else #f])
      ; if pattern is a list
      (cond
        [(empty? pattern) (cons '() tokens)]
        [(equal? 'SEQ (car pattern)) (seq-match (cdr pattern) tokens)]
        [(equal? 'ALT (car pattern)) (alt-match (cdr pattern) tokens)]
        [(equal? 'REPEAT (car pattern)) (repeat-match (cdr pattern) tokens)]
        [(equal? 'OPT (car pattern)) (opt-match (cdr pattern) tokens)])))

(define (parser [pattern  'CompUnit] [tokens tokens])
  (let ([ast (parser-recur pattern tokens)])
    (if ast
        ast
        (error "parser fail"))))

;(writeln (parser 'CompUnit tokens))