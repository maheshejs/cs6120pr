#lang racket
(require json)
(require racket/pretty)

;; Helpers
(define (label? lin)
  (hash-has-key? lin 'label))  

(define (insn? lin)
  (hash-has-key? lin 'op))  

;; Histogram construction
(define (process-op op acc)
  (if (hash-has-key? acc op) 
    (hash-set acc op (add1 (hash-ref acc op)))
    (hash-set acc op 1)))

(define (process-lin lin acc)
  (match lin
    [(? label? lin) 
     acc]
    [(? insn? lin)
     (process-op (hash-ref lin 'op) acc)]))  

(define program (read-json (current-input-port)))
(define functions (hash-ref program 'functions))

(for ([f functions])
  (printf "Function: ~a\n" (hash-ref f 'name))
  (define acc (foldl process-lin (hash) (hash-ref f 'instrs)))
  ;;sort keys alphabetically and print
  (for ([op (sort (hash-keys acc) string<?)]) 
    (printf "~a:~a\n" op (hash-ref acc op))))


