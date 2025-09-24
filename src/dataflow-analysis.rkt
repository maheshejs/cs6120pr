#lang racket
(require
  graph
  data/queue
  "lang-util.rkt")

(provide
 worklist)

(define (worklist direction lattice meet transfer cfg)
  (define q (make-queue))
  (define outs 
    (for/fold ([outs (hash)]) 
      ([v (get-vertices cfg)]) 
      (enqueue! q v) 
      (hash-set outs v (lattice v))))

  (let loop ([outs outs])
    (if (queue-empty? q)
      outs
      (let* ([v (dequeue! q)]
             [out (hash-ref outs v)]
             [fns ((if (forward? direction) get-predecessors get-successors) cfg v)]
             [bns ((if (forward? direction) get-successors get-predecessors) cfg v)]
             [in (meet (map (curry hash-ref outs) fns))])
        (unless (equal? out (transfer v in))
          (for ([w bns]) 
            (enqueue! q w)))
        (loop (hash-set outs v (transfer v in)))))))
