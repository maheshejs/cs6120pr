#lang racket
(require
  graph
  "dataflow-analysis.rkt"
  "lang-util.rkt")

(provide
 undead-analysis)

(define (undead-analysis program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map undead-func))]))

(define (undead-func func)
  (define cfg (hash-ref func 'cfg))
  (define bbs (hash-ref func 'bbs))
  ;; use dataflow analysis to get set of undeads for dead code elimination
  (define ust-ins (worklist "backward" 
                            (set) 
                            (λ (ss) 
                              (foldl set-union (set) ss)) 
                            (λ (k s) 
                              (define lins (hash-ref bbs k)) 
                              (car (foldr undead-lin (cons s empty) lins))) 
                            cfg))
  (define ust-outs 
    (for/hash ([k (get-vertices cfg)])
      (values k (foldl set-union (set) (map (curry hash-ref ust-ins) (get-successors cfg k))))))

  (hash-update func 'bbs (curryr undead-bbs ust-outs)))

(define (undead-bbs bbs ust-outs)
  (for/fold ([acc bbs]) 
    ([k (in-hash-keys bbs)]) 
    (hash-update acc k (λ (lins) 
                         (map cons lins (cdr (foldr undead-lin (cons (hash-ref ust-outs k) empty) lins)))))))

(define (undead-lin lin acc)
  (define ust-in (car acc))
  (match lin
    [(? label? lin) (cons ust-in acc)]
    [(? insn? lin)  (undead-insn lin acc)]))

(define (undead-insn insn acc) 
  (define ust-in (car acc))
  (define args (hash-ref insn 'args '()))
  (match insn
    [(? dest? insn) 
     (define dest (hash-ref insn 'dest))
     (define ust-out
       (for/fold ([acc (set-remove ust-in dest)]) 
         ([arg args])
         (set-add acc arg)))
     (cons ust-out acc)]
    [else  
     (define ust-out
       (for/fold ([acc ust-in]) 
         ([arg args])
         (set-add acc arg)))
     (cons ust-out acc)]))
