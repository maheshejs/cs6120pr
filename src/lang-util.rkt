#lang racket

(provide
 terminator?
 branch?
 commutative?
 alloc?
 id?
 const?
 effect?
 constant?
 value?
 dest?
 label?
 insn?
 label->string
 insn->string
 fresh
 print-label
 print-insn
 print-lin
 print-bb)

(define (terminator? op)
  (member (string->symbol op) '(br jmp ret)))

(define (branch? op)
  (member (string->symbol op) '(br jmp)))

(define (commutative? op)
  (member (string->symbol op) '(add mul and or eq ceq)))

(define (alloc? op)
  (member (string->symbol op) '(alloc)))

(define (id? op)
  (member (string->symbol op) '(id)))

(define (const? op)
  (member (string->symbol op) '(const)))

(define (dest? insn)
  (hash-has-key? insn 'dest))  

(define (label? lin)
  (hash-has-key? lin 'label))  

(define (insn? lin)
  (hash-has-key? lin 'op))  

(define (effect? lin)
  (and (insn? lin) (not (dest? lin))))  

(define (constant? lin)
  (and (insn? lin) (dest? lin) (const? (hash-ref lin 'op))))  

(define (value? lin)
  (and (insn? lin) (dest? lin) (not (const? (hash-ref lin 'op)))))

(define (label->string label)
  (hash-ref label 'label))  

(define (insn->string insn)
  (hash-ref insn 'op))  

(define fresh
  (let ([counter (let ([x 0]) 
                   (lambda () 
                     (set! x (add1 x)) 
                     x))]) 
    (lambda ([x 'tmp]) 
      (format "~a.~a" x (counter)))))

(define (print-label label)
  (printf "~a, " (hash-ref label 'label)))

(define (print-insn insn)
  (printf "~a, " (hash-ref insn 'op)))

(define (print-lin lin)
  (match lin
    [(? label? lin) (print-label lin)]
    [(? insn? lin)  (print-insn lin)]))

(define (print-bb bbs)
  (define sorted-keys (sort (hash-keys bbs) <))
  (for ([key sorted-keys])
    (define lins (hash-ref bbs key))
    (printf "~a:\t" key)
    (for-each print-lin lins)
    (printf "\n")))
