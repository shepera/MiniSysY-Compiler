#lang racket
(require racket/set)

(provide lexer)
(provide token)
(provide token-type)

(struct token (type value) #:transparent)
(struct token-reg (type reg) #:transparent)

; set of keywords
(define keywords (call-with-input-file "./grammer/keywords"
                   (lambda (in) (list->set (read in)))))

; list of token-reg to match
(define regs (map
              (lambda (str)
                (let* ([line (string-split str)]
                       [type (string->symbol (list-ref line 0))]
                       [reg (regexp (string-append "^" (list-ref line 1)))]) ; try to match the first word from the input
                  (token-reg type reg)))
              (file->lines "./grammer/lexer")))
  

(define (lexer [program "./program_input"])
  (call-with-input-file program
    (lambda (in)
      ; get token list in reverse order
      (define (loop tokens)
        ; skip comment //
        (cond 
          [(regexp-try-match #px"^//" in)
           (regexp-match #px"\n" in)
           (loop tokens)]
          ; skip comment /* */
          [(regexp-try-match #px"^/\\*" in)
           (if (regexp-try-match #px"\\*/" in)
               (loop tokens)
               (error "unterminated /* comment"))]
          ; skip white space
          [ (regexp-try-match #px"^[\\s]" in)
            (loop tokens)]
          ; match token
          [else
           (let reg-loop ([reg-list regs])
             ; can't match any token-reg
             (if (empty? reg-list)
                 (let ([b (read in)])
                   (if (eof-object? b)
                       tokens
                       (error b)))
                 ; try to match token
                 (let* ([reg (car reg-list)]
                        [match (regexp-try-match (token-reg-reg reg) in)])
                   (if match
                       (let ([match-string (bytes->string/utf-8 (list-ref match 0))])
                         (loop
                          (cons (cond
                                  [(equal? (token-reg-type reg) 'Ident)
                                   (if (set-member? keywords match-string)
                                       (token (string->symbol (string-titlecase match-string)) match-string)
                                       (token 'Ident match-string))]
                                  [(equal? (token-reg-type reg) 'Hex)
                                   (token
                                    'Number
                                    (string->number (substring match-string 2) 16))]
                                  [(equal? (token-reg-type reg) 'Octal)
                                   (token 'Number (string->number match-string 8))]
                                  [(equal? (token-reg-type reg) 'Decimal)
                                   (token 'Number (string->number match-string))]
                                  [else (token (token-reg-type reg) match-string)])
                                tokens)))
                       (reg-loop (cdr reg-list))))))]))
      (reverse (loop '())))))


;(writeln (lexer "./program_input"))