#lang racket
(require 
  data/queue
  graph 
  "dominator.rkt"
  "lang-util.rkt")

(provide
 hoist-invariants)

(define (hoist-invariants program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map hoist-func))]))

(define (hoist-func func)
  (define cfg (hash-ref func 'cfg))
  (define bbs (hash-ref func 'bbs))
  (define loops (hash-ref func 'loops))
  (define taints (hash-ref func 'taints))
  (define xs (hash-keys bbs))
  (hash-update func 'bbs (λ (bbs) (hoist-bbs xs bbs loops taints cfg))))

(define (hoist-bbs xs bbs loops taints cfg)
  (for/fold ([acc bbs])
    ([x xs])
    (for/fold ([acc acc])
      ([(lin i) (in-indexed (hash-ref acc x))])
      (match lin 
        [(? label? label) 
         acc] 
        [(? insn? insn) 
         (match insn 
           [(? phi? phi)
            acc] 
           [(? undef? undef) 
            acc] 
           [(? constant? constant) 
            (for/fold ([acc acc])
              ([k (in-hash-keys taints)])
              (if (set-member? (hash-ref taints k) (hash-ref constant 'dest))
                (let ([h (first (hash-ref loops k))]
                      [b (second (hash-ref loops k))])
                  (for/fold ([acc (hash-update acc x (λ (lins) 
                                                       (list-update lins i (λ (lin) 
                                                                             (hasheq 'op "nop")))))])
                    ([y (remove* b (get-predecessors cfg h))])
                    (hash-update acc y (curry hoist-constant constant))))
                acc))] 
           [(? value? value) 
            (for/fold ([acc acc])
              ([k (in-hash-keys taints)])
              (if (set-member? (hash-ref taints k) (hash-ref value 'dest))
                (let ([h (first (hash-ref loops k))]
                      [b (second (hash-ref loops k))])
                  (for/fold ([acc (hash-update acc x (λ (lins) 
                                                       (list-update lins i (λ (lin) 
                                                                             (hasheq 'op "nop")))))])
                    ([y (remove* b (get-predecessors cfg h))])
                    (hash-update acc y (curry hoist-value value))))
                acc))] 
           [(? effect? effect) 
            acc])]))))

(define (hoist-constant constant lins)
  (define-values (first-lins second-lins) (splitf-at lins (λ (lin)
                                                            (if (insn? lin)
                                                              (not (terminator? (hash-ref lin 'op)))
                                                              #t))))
  (append first-lins (list constant) second-lins))

(define (hoist-value value lins)
  (define-values (first-lins second-lins) (splitf-at lins (λ (lin)
                                                            (if (insn? lin)
                                                              (not (terminator? (hash-ref lin 'op)))
                                                              #t))))
  (append first-lins (list value) second-lins))
