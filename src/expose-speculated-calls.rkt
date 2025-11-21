#lang racket
(require 
  "lang-util.rkt")

(provide
 expose-speculated-calls)

(define (expose-speculated-calls program traces)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map expose-func))]))

(define (expose-func func)
  (define name (hash-ref func 'name (void)))
  (define args (hash-ref func 'args '()))
  (define types
    (for/hash ([arg args])
      (values (hash-ref arg 'name) (hash-ref arg 'type))))
  (define acc (foldl expose-lin (list 0 (hash) types name) (hash-ref func 'instrs)))
  (define bbs (second acc))
  (hash-set (hash-remove func 'instrs) 'bbs bbs))

(define (expose-lin lin acc)
  (match lin
    [(? label? lin) (expose-label lin acc)]
    [(? insn? lin)  (expose-insn lin acc)]))

(define (expose-insn insn acc)
  (define key (first acc))
  (define bbs (second acc))
  (define types (third acc))
  (define name (fourth acc))
  (define bb (hash-ref bbs key '()))
  (define dest (hash-ref insn 'dest (void)))
  (match (insn->string insn)
    [(? call? op)
     (cond
       [(string=? name "main")
     (define type (hash-ref insn 'type (void)))
     (define args (hash-ref insn 'args '()))
     (define cc-param-insns (for/foldr ([acc empty]) 
                            ([(arg i) (in-indexed args)]) 
                            (cons (hasheq 'op "id" 
                                          'type (hash-ref types arg)
                                          'args (list arg) 
                                          'dest (format "cc-param-~a" i)) acc)))
     (define cc-ret-insn 
       (if (value? insn)
         (hasheq 'op "id" 'type type 'args (list "cc-ret") 'dest dest)
         (hasheq 'op "nop")))
     (define key* (add1 key))
     (define key** (add1 key*))
     (define label* (fresh 'label))
     (define label** (fresh 'label))
     (define insns 
       (append
         (list 
           (hasheq 'op "const" 'type "bool" 'dest "cc-inl" 'value #f) 
           (hasheq 'op "speculate")
           #;(if (value? insn)
             (hasheq 'op "const" 'type type 'dest "cc-ret" 'value (if (string=? type "int") 0 #f))
             (hasheq 'op "nop"))
           )
         cc-param-insns
         (list
           #;(hash-remove (hash-remove (hash-remove (hash-update insn 'op (const "icall")) 'dest) 'type) 'args) 
           (hash-update insn 'op (const "icall"))
           )
         #;(list
           cc-ret-insn)
         (list 
           (hasheq 'op "guard" 'type "bool" 'args (list "cc-inl") 'labels (list label*))
           (hasheq 'op "commit") 
           (hasheq 'op "br" 'type "bool" 'args (list "cc-inl") 'labels (list label** label*)))))
     (list key**
           (hash-set (hash-set (hash-set bbs key (append bb insns)) 
                               key* 
                               (list (hasheq 'label label*) insn))
                     key** 
                     (list (hasheq 'label label**)))
           (if (value? insn) 
             (hash-set types dest (hash-ref insn 'type))
             types)
           name)
        ]
       [else
         (list (identity key) 
            (hash-set bbs key (append bb (list insn)))
            (if (or (value? insn) (constant? insn)) 
              (hash-set types dest (hash-ref insn 'type))
              types)
            name)])]
    [(? terminator? op)
     (list (add1 key) (hash-set bbs key (append bb (list insn))) types name)]
    [else
      (list (identity key) 
            (hash-set bbs key (append bb (list insn)))
            (if (or (value? insn) (constant? insn)) 
              (hash-set types dest (hash-ref insn 'type))
              types)
            name)]))

(define (expose-label label acc)
  (define key (first acc))
  (define bbs (second acc))
  (define types (third acc))
  (define name (fourth acc))
  (define bb (hash-ref bbs key '()))
  (if (empty? bb)
    (list (identity key) (hash-set bbs key (list label)) types name)
    (if (andmap label? bb) 
      (list (identity key) (hash-set bbs key (append bb (list label))) types name)
      (list (add1 key) (hash-set bbs (add1 key) (list label)) types name))))
