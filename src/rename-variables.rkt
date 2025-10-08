#lang racket
(require 
  data/queue
  graph 
  "dominator.rkt"
  "lang-util.rkt")

(provide
 rename-variables)

(define (rename-variables program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map rename-func))]))

(define (rename-func func)
  (define cfg  (hash-ref func 'cfg))
  (define args (hash-ref func 'args '()))
  (define defs (hash-ref func 'defs))
  (define stacks
    (for/hash ([var (map cdr (sequence->list (in-hash-keys defs)))])
      (values var (make-queue))))
  (for ([var (map (curryr hash-ref 'name) args)])
    (enqueue-front! (hash-ref stacks var) var))
  (hash-remove (hash-update func 'bbs (λ (bbs) (rename-bb 'entry bbs cfg stacks))) 'defs)) 

(define (rename-bb x bbs cfg stacks)
  (define acc 
    (for/fold ([acc bbs])
      ([(lin i) (in-indexed (hash-ref bbs x))]) 
      (match lin 
        [(? label? label) 
         acc] 
        [(? insn? insn) 
         (match insn 
           [(? phi? phi) 
            (hash-update acc x (λ (lins) 
                                 (list-update lins i (λ (lin) 
                                                       (rename-dest phi stacks)))))] 
           [(? undef? undef) 
            (hash-update acc x (λ (lins) 
                                 (list-update lins i (λ (lin) 
                                                       (rename-dest undef stacks)))))] 
           [(? constant? constant) 
            (hash-update acc x (λ (lins) 
                                 (list-update lins i (λ (lin) 
                                                       (rename-dest constant stacks)))))] 
           [(? value? value) 
            (hash-update acc x (λ (lins) 
                                 (list-update lins i (λ (lin) 
                                                       (rename-dest (rename-args value stacks) stacks)))))] 
           [(? effect? effect) 
            (hash-update acc x (λ (lins) 
                                 (list-update lins i (λ (lin) 
                                                       (rename-args effect stacks)))))])])))

  (define new-acc 
    (for/fold ([acc acc])
      ([y (get-successors cfg x)])
      (for/fold ([acc acc])
        ([(lin i) (in-indexed (hash-ref acc y))]) 
        (match lin 
          [(? label? label) 
           acc] 
          [(? insn? insn) 
           (match insn 
             [(? phi? phi) 
              (define j 
                (for/first ([(label j) (in-indexed (hash-ref phi 'labels))]
                            [x* (get-predecessors cfg y)]
                            #:when (equal? x x*))
                  j))
              (hash-update acc y (λ (lins) 
                                   (list-update lins i (λ (lin) 
                                                         (rename-phi-arg phi j stacks)))))] 
             [(? undef? undef) 
              acc] 
             [(? constant? constant) 
              acc] 
             [(? value? value) 
              acc] 
             [(? effect? effect) 
              acc])]))))

  (begin0
    (for/fold ([acc new-acc])
      ([y (get-successors (get-dominator-tree cfg) x)])
      (rename-bb y acc cfg stacks))

    (for ([lin (hash-ref bbs x)]) 
      (match lin 
      [(? label? label) 
       (void)]
      [(? insn? insn) 
       (define dest (hash-ref insn 'dest (void))) 
       (match insn 
         [(? phi? phi) 
          (dequeue! (hash-ref stacks dest))] 
         [(? undef? undef) 
          (dequeue! (hash-ref stacks dest))] 
         [(? constant? constant) 
          (dequeue! (hash-ref stacks dest))] 
         [(? value? value) 
          (dequeue! (hash-ref stacks dest))] 
         [(? effect? effect) 
          (void)])]))))

(define (rename-dest insn stacks)
  (if (hash-has-key? insn 'dest)
    (hash-update insn 'dest (λ (dest) 
                              (let ([new-dest (fresh (string->symbol dest))]) 
                                (enqueue-front! (hash-ref stacks dest) new-dest) 
                                new-dest)))
    insn))

(define (rename-args insn stacks)
  (if (hash-has-key? insn 'args)
    (hash-update insn 'args (λ (args) 
                              (for/foldr ([acc empty]) 
                                         ([arg args]) 
                                         (cons (peek! (hash-ref stacks arg)) acc)))) 
    insn))

(define (rename-phi-arg phi i stacks)
  (hash-update phi 'args (λ (args) 
                           (list-update args i (λ (arg) 
                                                 (peek! (hash-ref stacks arg)))))))

(define (peek! q)
  (define e (dequeue! q))
  (enqueue-front! q e)
  e)
