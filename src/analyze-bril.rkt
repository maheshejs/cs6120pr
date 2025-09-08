#lang racket
(require json)
(require racket/pretty)

(define program (read-json (current-input-port)))

(define functions (hash-ref program 'functions))

(define (process-op o acc)
  (if (hash-has-key? acc o) (hash-set acc o (add1 (hash-ref acc o)))
    (hash-set acc o 1)))

(define (process-instruction i acc)
  (match i
    [(hash 'label _ #:open) 
     acc]
    [(hash 'op _ #:open)
     (process-op (hash-ref i 'op) acc)]))  

(for ([f functions])
  (printf "Function: ~a\n" (hash-ref f 'name))
  (define acc (foldl process-instruction (hash) (hash-ref f 'instrs)))
  ;;sort keys alphabetically and print
  (for ([key (sort (hash-keys acc) string<?)]) 
    (printf "~a:~a\n" key (hash-ref acc key))))


