#lang racket
(require 
  graph 
  "lang-util.rkt")

(provide
 decorate-flow-graph)

(define (decorate-flow-graph program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map decorate-func))]))

(define (decorate-func func)
  (define bbs (hash-ref func 'bbs))
  (define cfg (unweighted-graph/directed '()))
  (for-each (curry add-vertex! cfg) (hash-keys bbs))
  ;; Add edges based on last in each block
  (for ([(key lins) (in-hash bbs)])
    (decorate-lin (last lins) key cfg bbs))
  (hash-set func 'cfg cfg))

(define (decorate-lin lin key cfg bbs)
  (match lin
    [(? label? lin) (void)]
    [(? insn? lin)  (decorate-insn lin key cfg bbs)]))

(define (decorate-insn insn key cfg bbs)
  (match (hash-ref insn 'op)
    [(? branch? op) 
     (define pred key)
     (define succs (map (curryr label->bb-key bbs) (hash-ref insn 'labels)))
     (for-each (curry add-directed-edge! cfg pred) succs)]
    [else
     (define pred key)
     (define succ (add1 pred))
     (and (< succ (hash-count bbs)) (add-directed-edge! cfg pred succ))]))

(define (label->bb-key label-string bbs)
  (for/first ([(key lins) (in-hash bbs)]
              #:when (let ([labels (filter label? lins)])
                           (ormap (compose (curry equal? label-string) label->string) labels)))
    key))

