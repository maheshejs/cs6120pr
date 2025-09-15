#lang racket
(require 
  "lang-util.rkt")

(provide
 expose-basic-blocks)

(define (expose-basic-blocks program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map expose-func))]))

(define (expose-func func)
  (define bbs (cdr (foldl expose-lin (cons 0 (hash)) (hash-ref func 'instrs))))
  (hash-set (hash-remove func 'instrs) 'bbs bbs))

(define (expose-lin lin acc)
  (match lin
    [(? label? lin) (expose-label lin acc)]
    [(? insn? lin)  (expose-insn lin acc)]))

(define (expose-insn insn acc)
  (define key (car acc))
  (define bbs (cdr acc))
  (define bb (hash-ref bbs key '()))
  (match (insn->string insn)
    [(? terminator? op)
     (cons (add1 key) (hash-set bbs key (append bb (list insn))))]
    [else
     (cons (identity key) (hash-set bbs key (append bb (list insn))))]))

(define (expose-label label acc)
  (define key (car acc))
  (define bbs (cdr acc))
  (define bb (hash-ref bbs key '()))
  (if (empty? bb)
    (cons (identity key) (hash-set bbs key (list label)))
    (if (andmap label? bb) 
      (cons (identity key) (hash-set bbs key (append bb (list label))))
      (cons (add1 key) (hash-set bbs (add1 key) (list label))))))
