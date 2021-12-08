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
      (cdr x))
     (display "\n"))
   
   (cdr func-list))
  
  (display "}\n")
  )
                

(define (print-llvm-ir ir-list)
  ;print declare
  (for-each
   (lambda (x)
     (map (lambda (x) (display x) (display " ")) x)
     (display "\n"))
   (car ir-list))
  ;print function
  (for-each
   (lambda (x)  (print-function x))
   (cdr ir-list)))

(print-llvm-ir ir-list)