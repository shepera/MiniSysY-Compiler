#lang racket

(provide make-counter)
(provide try-hash-set!)

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