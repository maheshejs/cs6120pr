#lang racket
(require json)
(require graph)
(require racket/pretty)
(require racket/system)

(define program (read-json (current-input-port)))

(define functions (hash-ref program 'functions))

(define (terminator? t)
  (member (string->symbol t) '(br jmp ret)))

(define (branch? b)
  (member (string->symbol b) '(br jmp)))

(define (is-label? lin)
  (match lin
    [(hash 'label _ #:open) 
     #t]
    [(hash 'op _ #:open)
     #f]))  

(define (expose-bb-label l state)
  (printf "  label: ~a\n" (hash-ref l 'label))
  (define key (car state))
  (define bb (cdr state))
  (if (hash-has-key? bb key)
    (if (andmap is-label? (hash-ref bb key)) 
      (cons key (hash-set bb key (append (hash-ref bb key '()) (list l))))
      (cons (add1 key) (hash-set bb (add1 key) (list l))))
    (cons key (hash-set bb key (list l)))))

(define (expose-bb-insn i state)
  (printf "  op: ~a\n" (hash-ref i 'op))
  (define key (car state))
  (define bb (cdr state))
  (match (hash-ref i 'op)
    [(? terminator? t)
     (cons (add1 key) (hash-set bb key (append (hash-ref bb key '()) (list i))))]
    [_
     (cons key (hash-set bb key (append (hash-ref bb key '()) (list i))))]))

(define (expose-bb-lin lin state)
  (match lin
    [(hash 'label _ #:open) 
     (expose-bb-label lin state)]
    [(hash 'op _ #:open)
     (expose-bb-insn lin state)]))  

(define (print-label l)
  (printf "~a, " (hash-ref l 'label)))

(define (print-insn i)
  (printf "~a, " (hash-ref i 'op)))

(define (print-lin lin)
  (match lin
    [(hash 'label _ #:open) 
     (print-label lin)]
    [(hash 'op _ #:open)
     (print-insn lin)]))

(define (print-bb bb) 
  (for ([(key lins) (in-hash bb)])
    (printf "~a:\t" key)
    (for-each print-lin lins)
    (printf "\n")))

(define (label->key lbl bb)
  (for/first ([(key lins) (in-hash bb)]
              #:when (and (hash-has-key? (first lins) 'label) 
                          (equal? (hash-ref (first lins) 'label) lbl)))
    key))

(define (update-cfg-label g bb key l)
  (void))

(define (update-cfg-insn g bb key i)
  (match (hash-ref i 'op)
    [(? branch? b) 
     (define pred key)
     (define succs (map (curryr label->key bb) (hash-ref i 'labels)))
     (map (curry add-directed-edge! g pred) succs)]
    [_
     (define pred key)
     (define succ (add1 pred))
     (and (< succ (hash-count bb)) (add-directed-edge! g pred succ))]))

(define (update-cfg-lin g bb key lin)
  (match lin
    [(hash 'label _ #:open) 
     (update-cfg-label g bb key lin)]
    [(hash 'op _ #:open)
     (update-cfg-insn g bb key lin)]))

(for ([f functions])
  (printf "-------------------------------\n")
  (printf "FUNCTION: ~a\n" (hash-ref f 'name))
  (printf "-------------------------------\n")
  (define bb (cdr (foldl expose-bb-lin (cons 0 (hash)) (hash-ref f 'instrs))))
  (printf "------------\n")
  (printf "BASIC BLOCKS\n")
  (printf "------------\n")
  (print-bb bb)
  (define g (unweighted-graph/directed '()))
  (map (curry add-vertex! g) (hash-keys bb))
  (for ([(key lins) (in-hash bb)])
    (update-cfg-lin g bb key (last lins)))
  (printf "------------------\n")
  (printf "CONTROL-FLOW GRAPH\n")
  (printf "------------------\n")
  (graphviz g #:output (current-output-port))
  (call-with-output-file "graph.dot" 
                         (λ (out) (graphviz g #:output out)) #:exists 'replace))

