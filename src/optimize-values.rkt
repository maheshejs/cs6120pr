#lang racket
(require 
  "lang-util.rkt")

(provide optimize-values)

(define (optimize-values program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map optimize-func))]))

(define (optimize-func func)
  (hash-update func 'bbs optimize-bbs))

(define (optimize-bbs bbs)
  (for/fold ([acc bbs]) 
    ([k (in-hash-keys bbs)]) 
    (hash-update acc k optimize-lins)))

(define (optimize-lins lins)
  (define env (make-hash))
  (let loop ([lin   (first lins)]
             [tail  (rest lins)]
             [table (hash)]
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
    [(? effect?   insn) (optimize-effect   insn tail table env)]
    [(? constant? insn) (optimize-constant insn tail table env)]
    [(? value?    insn) (optimize-value    insn tail table env)]))

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
             (hash-set! env dest (cons new-dest (void)))
             (values new-value (hash-set table key new-dest)))))]))

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
