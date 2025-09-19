#lang racket
(require
  graph
  data/queue
  "lang-util.rkt")

(provide
 worklist)

(define (worklist direction l meet transfer cfg)
  (define q (make-queue))
  (define outs (make-hash))
  (for ([v (get-vertices cfg)]) 
    (enqueue! q v) 
    (hash-set! outs v l))
  (let loop ()
    (when (not (queue-empty? q))
      (define v (dequeue! q))
      (define out (hash-ref outs v))
      (define fns ((if (forward? direction) get-predecessors get-successors) cfg v))
      (define bns ((if (forward? direction) get-successors get-predecessors) cfg v))
      (define in (meet (map (curry hash-ref outs) fns)))
      (when (not (equal? out (transfer v in)))
        (for ([w bns])
          (enqueue! q w)))
      (hash-set! outs v (transfer v in))
      (loop)))
  outs)
