#lang racket
(require json)
(require graph)
(require racket/pretty)
(require racket/system)

;; Helpers
(define (terminator? op)
  (member (string->symbol op) '(br jmp ret)))

(define (branch? op)
  (member (string->symbol op) '(br jmp)))

(define (label? lin)
  (hash-has-key? lin 'label))  

(define (insn? lin)
  (hash-has-key? lin 'op))  

(define (label->string label)
  (hash-ref label 'label))  

(define (insn->string insn)
  (hash-ref insn 'op))  

;; Printing 
(define (print-label label)
  (printf "~a, " (hash-ref label 'label)))

(define (print-insn insn)
  (printf "~a, " (hash-ref insn 'op)))

(define (print-lin lin)
  (match lin
    [(? label? lin) (print-label lin)]
    [(? insn? lin)  (print-insn lin)]))

(define (print-bb bbs)
  (define sorted-keys (sort (hash-keys bbs) <))
  (for ([key sorted-keys])
    (define lins (hash-ref bbs key))
    (printf "~a:\t" key)
    (for-each print-lin lins)
    (printf "\n")))

;; Basic block construction
(define (expose-bb-label label acc)
  (printf "  label: ~a\n" (label->string label))
  (define key (car acc))
  (define bbs (cdr acc))
  (define bb (hash-ref bbs key '()))
  (if (empty? bb)
    (cons (identity key) (hash-set bbs key (list label)))
    (if (andmap label? bb) 
      (cons (identity key) (hash-set bbs key (append bb (list label))))
      (cons (add1 key) (hash-set bbs (add1 key) (list label))))))

(define (expose-bb-insn insn acc)
  (printf "  op: ~a\n" (insn->string insn))
  (define key (car acc))
  (define bbs (cdr acc))
  (define bb (hash-ref bbs key '()))
  (match (insn->string insn)
    [(? terminator? op)
     (cons (add1 key) (hash-set bbs key (append bb (list insn))))]
    [else
     (cons (identity key) (hash-set bbs key (append bb (list insn))))]))

(define (expose-bb-lin lin acc)
  (match lin
    [(? label? lin) (expose-bb-label lin acc)]
    [(? insn? lin)  (expose-bb-insn lin acc)]))  

;; Control flow graph construction
(define (label->bb-key label-string bbs)
  (for/first ([(key lins) (in-hash bbs)]
              #:when (let ([labels (filter label? lins)])
                           (ormap (compose (curry equal? label-string) label->string) labels)))
    key))

(define (update-cfg-insn insn key g bbs)
  (match (hash-ref insn 'op)
    [(? branch? op) 
     (define pred key)
     (define succs (map (curryr label->bb-key bbs) (hash-ref insn 'labels)))
     (for-each (curry add-directed-edge! g pred) succs)]
    [else
     (define pred key)
     (define succ (add1 pred))
     (and (< succ (hash-count bbs)) (add-directed-edge! g pred succ))]))

(define (update-cfg-lin lin key g bbs)
  (match lin
    [(? label? lin) (void)]
    [(? insn? lin)  (update-cfg-insn lin key g bbs)]))

(define (make-cfg bbs)
  (define g (unweighted-graph/directed '()))
  (for-each (curry add-vertex! g) (hash-keys bbs))
  ;; Add edges based on last in each block
  (for ([(key lins) (in-hash bbs)])
    (update-cfg-lin (last lins) key g bbs))
  g)

;; Parse input program from stdin
(define program (read-json (current-input-port)))
(define functions (hash-ref program 'functions))

;; Main function
(for ([f functions])
  (printf "-------------------------------\n")
  (printf "FUNCTION: ~a\n" (hash-ref f 'name))
  (printf "-------------------------------\n")
  (define bbs (cdr (foldl expose-bb-lin (cons 0 (hash)) (hash-ref f 'instrs))))
  (printf "------------\n")
  (printf "BASIC BLOCKS\n")
  (printf "------------\n")
  (print-bb bbs)
  (printf "------------------\n")
  (printf "CONTROL-FLOW GRAPH\n")
  (printf "------------------\n")
  (define g (make-cfg bbs))
  (graphviz g #:output (current-output-port))
  (call-with-output-file "graph.dot" (lambda (out) (graphviz g #:output out)) #:exists 'replace))

