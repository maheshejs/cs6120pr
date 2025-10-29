#lang racket
(require 
  graph
  "dominator.rkt"
  "lang-util.rkt")

(provide
 taint-invariants)

(define (taint-invariants program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map taint-func))]))

(define (taint-func func)
  (define cfg   (hash-ref func 'cfg))
  (define bbs   (hash-ref func 'bbs))
  (define loops (hash-ref func 'loops))
  (define d-graph (unweighted-graph/directed '()))
  (define dominators (get-dominators cfg))
  (for ([(v d) (in-hash dominators)])
    (for ([w d]) 
      (add-directed-edge! d-graph w v)))
  (hash-update func 'taints (λ (taints) (taint-bbs bbs loops taints d-graph))))

(define (taint-bbs bbs loops taints d-graph)
  (for/fold ([acc taints]) 
    ([k (in-hash-keys bbs)])
    (taint-lins (hash-ref bbs k) loops acc d-graph)))

(define (taint-lins lins loops taints d-graph)
  (for/fold ([acc taints]) 
    ([lin lins])
    (taint-lin lin loops acc d-graph)))

(define (taint-lin lin loops taints d-graph)
  (match lin
    [(? label? lin) (taint-label lin loops taints d-graph)]
    [(? insn?  lin) (taint-insn  lin loops taints d-graph)]))

(define (taint-label label loops taints d-graph)
  taints)

(define (taint-insn insn loops taints d-graph) 
  (match insn
    [(? effect?   insn) 
     (taint-effect   insn loops taints d-graph)]
    [(? phi? insn) 
     (taint-phi      insn loops taints d-graph)]
    [(? undef? insn) 
     (taint-undef    insn loops taints d-graph)]
    [(? constant? insn) 
     (taint-constant insn loops taints d-graph)]
    [(? value?    insn) 
     (taint-value    insn loops taints d-graph)]))

(define (taint-effect effect loops taints d-graph) 
  taints)

(define (taint-phi phi loops taints d-graph) 
  taints)

(define (taint-undef undef loops taints d-graph) 
  taints)

(define (taint-constant constant loops taints d-graph) 
  (define dest (hash-ref constant 'dest))
  (define bb-key-dest (extract-bb-key dest))
  (for/fold ([acc taints])
    ([k (in-hash-keys loops)])
    (define loop-list (second (hash-ref loops k)))
    (define exit-list (third (hash-ref loops k)))
    (define ss (map (curry get-predecessors d-graph) exit-list)) 
    (define s-int (if (empty? ss) (set) (apply set-intersect ss)))
    (if (and (member bb-key-dest loop-list) (member bb-key-dest s-int))
      (if (hash-has-key? acc k)
        (hash-update acc k (λ (st) (set-add st dest)))
        (hash-set acc k (set dest)))
      acc)))

(define (taint-value value loops taints d-graph) 
  (define dest (hash-ref value 'dest))
  (define args (hash-ref value 'args))
  (define bb-key-dest (extract-bb-key dest))
  (for/fold ([acc taints])
    ([k (in-hash-keys loops)])
    (define loop-list (second (hash-ref loops k)))
    (define exit-list (third (hash-ref loops k)))
    (define ss (map (curry get-predecessors d-graph) exit-list)) 
    (define s-int (if (empty? ss) (list) (apply set-intersect ss)))
    (if (and (member bb-key-dest loop-list) (member bb-key-dest s-int))
      (if (for/fold ([res #t])
            ([arg args]) 
            (define bb-key-arg (extract-bb-key arg)) 
            (and res 
                 (if (not (member bb-key-arg loop-list)) 
                   #t
                   (if (hash-has-key? acc k) 
                     (set-member? (hash-ref acc k) arg)
                     #f))))
        (if (hash-has-key? acc k)
          (hash-update acc k (λ (st) (set-add st dest)))
          (hash-set acc k (set dest)))
        acc)
      acc)))

(define (extract-bb-key var) 
  (let ([s var]) 
    (let ([m (regexp-match #rx"([0-9]+)" s)]) 
      (and m (string->number (first m))))))
