#lang racket
(require "./ir-list-generator.rkt")

(define ir-list (ir-list-generator))

(define (print-function func-list)
  ; print function define
  (for-each
   (lambda (x)
     (display x) (display " "))
   (car func-list))
  (display "{\n")

  ; print body of the function
  (for-each
   (lambda (x)
     (for-each
      (lambda (word)
        (display word) (display " "))
      x)
     (display "\n"))
   
   (cdr func-list))
  
  (display "}\n")
  )
                

(define (print-llvm-ir ir-list)
  (for-each
   (lambda (x)  (print-function x))
   ir-list))

(print-llvm-ir ir-list)