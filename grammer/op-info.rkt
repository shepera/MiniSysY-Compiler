#lang racket

(provide operation)
(provide operation-ir)
(provide operation-operand-type)
(provide operation-ret-type)
(provide op-hash)

(struct operation (ir operand-type ret-type) #:transparent)

(define op-hash (hash 'Plus (operation 'add 'i32 'i32)
                      'Minus (operation 'sub 'i32 'i32)
                      'Mult (operation 'mul 'i32 'i32)
                      'Div (operation 'sdiv 'i32 'i32)
                      'Mod (operation 'srem 'i32 'i32)
                      'And (operation 'and 'i1 'i1)
                      'Or (operation 'or 'i1 'i1)
                      'Equal (operation '(icmp eq) 'i32 'i1)
                      'Ne (operation '(icmp ne) 'i32 'i1)
                      'Le (operation '(icmp sle) 'i32 'i1)
                      'Ge (operation '(icmp sge) 'i32 'i1)
                      'Lt (operation '(icmp slt) 'i32 'i1)
                      'Gt (operation '(icmp sgt) 'i32 'i1)))