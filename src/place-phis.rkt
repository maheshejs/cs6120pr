#lang racket
(require 
  racket/list
  data/queue
  graph
  "dominator.rkt"
  "lang-util.rkt")

(provide 
  place-phis)

(define (place-phis program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map place-func))]))

(define (place-func func) 
  (define cfg  (hash-ref func 'cfg))
  (define df (get-dominator-frontier cfg))
  (define n (length (get-vertices cfg)))
  (define q (make-queue))
  (define defs (hash-ref func 'defs))

  (define (update-acc acc node type-var) 
    (if (hash-has-key? acc node) 
      (hash-update acc node (λ (types-vars) (set-add types-vars type-var))) 
      (hash-set acc node (set type-var))))

  (define-values (iter-count acc has-already work)
    (for/fold ([iter-count 0]
               [acc (hash)]
               [has-already (hash)]
               [work (hash)])
      ([var  (map cdr (sequence->list (in-hash-keys defs)))]
       [type (map car (sequence->list (in-hash-keys defs)))])
      (let loop ([iter-count (add1 iter-count)]
                 [acc acc]
                 [has-already has-already]
                 [work (for/fold ([work work]) 
                         ([x (hash-ref defs (cons type var))]) 
                         (enqueue! q x) 
                         (hash-set work x (add1 iter-count)))])
        (if (queue-empty? q)
          (values iter-count
                  acc 
                  has-already 
                  work)
          (let ([x (dequeue! q)])
            (call-with-values 
              (λ ()
                (for/fold ([iter-count iter-count]
                           [acc acc] 
                           [has-already has-already] 
                           [work work]) 
                  ([y (hash-ref df x '())]) 
                  (if (< (hash-ref has-already y 0) iter-count) 
                    (if (< (hash-ref work y 0) iter-count) 
                      (begin0 
                        (values iter-count 
                                (update-acc acc y (cons type var)) 
                                (hash-set has-already y iter-count) 
                                (hash-set work y iter-count)) 
                        (enqueue! q y)) 
                      (values iter-count 
                              (update-acc acc y (cons type var)) 
                              (hash-set has-already y iter-count) 
                              work)) 
                    (values iter-count 
                            acc 
                            has-already
                            work)))) 
              loop))))))

  (hash-update func 'bbs (curryr place-bbs acc cfg)))

(define (place-bbs bbs phis cfg)
  (for/hash ([(key lins) (in-hash bbs)])
    (cond
      [(member key (sequence->list (in-hash-keys phis)))
        (define preds (get-predecessors cfg key))
        (define n (length preds))
        (define types-vars (hash-ref phis key))
        (define-values (first-lins second-lins) (splitf-at lins label?))
        (define inss 
          (for/list ([type-var (in-set types-vars)])
            (hasheq 'type (car type-var)
                    'dest (cdr type-var) 
                    'op "phi" 
                    'labels (map get-leader (map (curry hash-ref bbs) preds))
                    'args (make-list n (cdr type-var)))))
        (values key (append first-lins inss second-lins))]
      [else
        (values key lins)])))

(define (get-leader lins)
  (let ([lin (first lins)])
    (if (label? lin)
      (hash-ref lin 'label)
      "funky-empty")))
