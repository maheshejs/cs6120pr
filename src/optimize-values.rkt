#lang racket
(require 
  graph
  "dataflow-analysis.rkt"
  "lang-util.rkt")

(provide optimize-values)

(define (optimize-values program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map optimize-func))]))

(define (optimize-func func) 
  (define cfg (hash-ref func 'cfg)) 
  (define bbs (hash-ref func 'bbs)) 
  (define idst-outs (worklist "forward" 
                       (λ (k)
                        (let ([ss (for/list ([(k lins) (in-hash bbs)]) 
                                   (for/set ([value (filter value? lins)] 
                                             #:when (id? (hash-ref value 'op))) 
                                     (cons (hash-ref value 'type) 
                                           (cons (hash-ref value 'dest) (first (hash-ref value 'args))))))])
                         (if (empty? ss) (set) (apply set-union ss))))
                       (λ (ss) 
                         (if (empty? ss) (set) (apply set-intersect ss))) 
                       (λ (k s) 
                         (define (set-kill s d) 
                           (for/set ([p s] 
                                     #:when (not (or (string=? d (cadr p)) 
                                                     (string=? d (cddr p))))) 
                             p))
                         (define lins (hash-ref bbs k))
                         (for/fold ([acc s])
                           ([lin lins])
                           (match lin 
                             [(? label? label) 
                              acc]
                             [(? insn? insn) 
                              (define op   (hash-ref insn 'op (void))) 
                              (define dest (hash-ref insn 'dest (void))) 
                              (define args (hash-ref insn 'args (void)))
                              (define type (hash-ref insn 'type (void)))
                              (match insn 
                                [(? constant? constant) 
                                 (set-kill acc dest)] 
                                [(? value? value) 
                                 (match op 
                                   [(? id? op) 
                                    (set-add (set-kill acc dest) (cons type 
                                                                       (cons dest (first args))))] 
                                   [else 
                                     (set-kill acc dest)])] 
                                [(? effect? effect) 
                                 acc])]))) 
                       cfg)) 

  (define idst-ins
    (for/hash ([k (get-vertices cfg)]) 
      (let ([ss (map (curry hash-ref idst-outs) (get-predecessors cfg k))])
        (values k (if (empty? ss) (set) (apply set-intersect ss))))))

  (hash-update func 'bbs (curryr optimize-bbs idst-ins)))

(define (optimize-bbs bbs idst-ins)
  (for/fold ([acc bbs]) 
    ([k (in-hash-keys bbs)])
    (define idst-in (hash-ref idst-ins k))
    (hash-update acc k (curryr optimize-lins idst-in))))

(define (optimize-lins lins idst-in)
  (define env (make-hash))
  (define init-table
    (for/hash ([p (in-set idst-in)]) 
      (define type (car p)) 
      (define dest (cadr p)) 
      (define arg  (cddr p)) 
      (define new-dest (optimize-dest dest lins))
      (hash-set! env dest (cons arg (void)))
      (values (hasheq 'op "id" 'type type 'args (list arg)) arg)))

  (let loop ([lin   (first lins)]
             [tail  (rest lins)]
             [table init-table]
             [acc   empty]) 
    (if (empty? tail)
        (let*-values ([(opt-lin new-table) (optimize-lin lin tail table env)])
          (reverse (cons opt-lin acc)))
        (let*-values ([(new-lin) (first tail)]
                      [(new-tail) (rest tail)]
                      [(opt-lin new-table) (optimize-lin lin tail table env)])
          (loop new-lin new-tail new-table (cons opt-lin acc))))))

(define (optimize-lin lin tail table env)
  (match lin
    [(? label? lin) (optimize-label lin tail table env)]
    [(? insn?  lin) (optimize-insn  lin tail table env)]))

(define (optimize-label label tail table env)
  (values label table))

(define (optimize-insn insn tail table env) 
  (match insn
    [(? effect?   insn) 
     (optimize-effect   insn tail table env)]
    [(? constant? insn) 
     (for ([(k v) (in-hash env)] 
           #:when (string=? (hash-ref insn 'dest) (car v))) 
       (hash-remove! env k))
     (optimize-constant insn tail table env)]
    [(? value?    insn) 
     (for ([(k v) (in-hash env)] 
           #:when (string=? (hash-ref insn 'dest) (car v))) 
       (hash-remove! env k))
     (optimize-value    insn tail table env)]))

(define (optimize-effect effect tail table env)
  (define new-effect
    (hash-update effect 'args (λ (args) (map (curryr optimize-arg env) args)) empty))
  (values new-effect table))

(define (optimize-constant constant tail table env) 
  (let* ([dest (hash-ref constant 'dest)]
         [val  (hash-ref constant 'value)]
         [key (hash-remove constant 'dest)])
    (if (hash-has-key? table key)
        (let* ([var (hash-ref table key)])
          (hash-set! env dest (cons var val))
          (values constant table))
        (let* ([new-dest (optimize-dest dest tail)])
          (hash-set! env dest (cons new-dest val))
          (values (hash-update constant 'dest (const new-dest)) (hash-set table key new-dest))))))

(define (optimize-value value tail table env) 
  (define op (hash-ref value 'op))
  (match op
    [(? alloc? op) (values value table)]
    [else
     (let* ([dest (hash-ref value 'dest)]
            [args-updater (λ (args)
                            (map (curryr optimize-arg env)
                                 (if (commutative? op) (sort args string<?) args)))]
            [key
              (if (id? op)
                (let* ([arg (first (hash-ref value 'args))]
                       [k (var->key (car (hash-ref env arg (cons arg (void)))) table)])
                  (or k (hash-update (hash-remove value 'dest) 'args args-updater empty)))
                (hash-update (hash-remove value 'dest) 'args args-updater empty))])
       (if (hash-has-key? table key)
           (let* ([var (hash-ref table key)]
                  [new-value (optimize-id (hash-update 
                                            (hash-update 
                                              (hash-update 
                                                (hash-remove (hash-remove value 'funcs) 'labels) 
                                                'op (const "id")) 
                                              'args (const (list var)) empty) 
                                            'dest (const dest) empty) env)])
             (hash-set! env dest (cons var (void)))
             (values new-value table))

           (let* ([new-dest (optimize-dest dest tail)]
                  [new-value (hash-update
                               (hash-update value 'dest (const new-dest))
                               'args (λ (args) (map (curryr optimize-arg env) args))
                               empty)])
             (if (id? op)
               (hash-set! env dest (cons (first (hash-ref value 'args)) (void)))
               (hash-set! env dest (cons new-dest (void))))
             
             (values new-value (hash-set table key (if (id? op) (first (hash-ref value 'args)) new-dest))))))]))

(define (optimize-id id env)
  (define arg (first (hash-ref id 'args)))
  (define val (cdr (hash-ref env arg (cons arg (void)))))
  (define new-id
    (if (void? val)
      id
      (hash-set (hash-update (hash-remove id 'args) 'op (const "const")) 'value val)))
  new-id)

(define (optimize-dest dest tail)
  (if (ormap (λ (v) (string=? dest (hash-ref v 'dest)))
             (filter dest? tail))
      (fresh 'lvn)
      dest))

(define (optimize-arg arg env)
  (car (hash-ref env arg (cons arg (void)))))

(define (var->key var table)
  (for/first ([(k v) (in-hash table)]
              #:when (string=? var v))
    k))
