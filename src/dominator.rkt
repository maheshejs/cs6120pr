#lang racket
(require 
  data/queue
  graph 
  "dataflow-analysis.rkt"
  "lang-util.rkt")

(provide
 get-dominators
 get-dominator-tree
 get-dominator-frontier)

(define (entry? v)
  (eq? v 'entry))

(define (get-dominators g) 
  (worklist "forward"
            (λ (v)
              (if (entry? v)
                (set v)
                (list->set (get-vertices g))))
            (λ (ss)
              (if (empty? ss) (set) (apply set-intersect ss))) 
            (λ (v s)
              (set-add s v)) 
            g))

(define (get-dominator-tree g)
  (define t (unweighted-graph/directed '()))
  (define dss (get-dominators g))
  (for ([(v ds) (in-hash dss)])
    (unless (entry? v)
      (add-directed-edge!
        t
        (for/first ([w (in-set (set-remove ds v))]
                    #:when (= (set-count ds) (add1 (set-count (hash-ref dss w)))))
          w)
        v)))
  t)

(define (get-dominator-frontier-alt g)
  (define t (get-dominator-tree g))
  (for/fold ([f (for/hash ([v (get-vertices g)]) 
                  (values v (set)))]) 
    ([q (get-vertices g)] 
     #:when (> (length (get-predecessors g q)) 1)) 
    (for/fold ([f f]) 
      ([x (get-predecessors g q)]) 
      (let loop ([y x] [f f]) 
        (if (equal? y (first (get-predecessors t q))) 
          f 
          (loop (first (get-predecessors t y)) (hash-update f y (λ (s) (set-add s q)))))))))

(define (get-dominator-frontier g)
  (define d-graph (unweighted-graph/directed '()))
  (define dominators (get-dominators g))
  (for ([(v d) (in-hash dominators)])
    (for ([w d]) 
      (add-directed-edge! d-graph w v)))
  (for/hash ([k (get-vertices g)])
    (values k (list->set 
                (remove* 
                  (remove k (get-successors d-graph k)) 
                  (flatten (map (curry get-successors g) 
                                (get-successors d-graph k))))))))

;; ----------
;; Unit tests 
;; ----------
(module+ test
  (require rackunit)

  ;; Helpers 
  ;; build small graphs
  (define (make-graph edges)
    (unweighted-graph/directed edges))

  ;; check that dominators are necessary
  ;; For each vertex v and each dominator d of v,
  ;; remove d from the graph and verify that v
  ;; becomes unreachable from 'entry. 
  (define (check-dominator-property g dss) 
    (for ([(v ds) (in-hash dss)]) 
      (check-true (set-member? ds 'entry)
                  (format "vertex ~a is not dominated by entry" v))
      (check-true (set-member? ds v)
                  (format "vertex ~a is not dominated by itself" v))
      (for ([d (in-set (set-subtract ds (set 'entry v)))]) 
        (define g* (graph-copy g))
        (remove-vertex! g* d) 
        (define-values (distances _) (bfs g* 'entry))
        (let ([distance (hash-ref distances v -1)]) ;; -1 for missing vertex 
          (check-true (infinite? distance) 
                      (if (= -1 distance) 
                        (format "error: missing vertex ~a" v) 
                        (format "vertex ~a still reachable after removing dominator ~a" v d)))))))

  ;; Test graphs
  ;; Graph 1: straight line
  (define g1 (make-graph 
               '((entry a) (a b) (b c))))

  ;; Graph 2: diamond
  (define g2 (make-graph 
               '((entry a) (a b) (a c) (b d) (c d))))

  ;; Graph 3: loop back to entry
  (define g3 (make-graph
               '((entry a) (a b) (b a) (b c))))

  ;; Graph 4: small structured flow
  (define g4 (make-graph
               '((entry a) (a b) (a f) (b c) (b d) 
                 (c e) (d e) (e f))))

  ;; Graph 5: bigger loop
  (define g5 (make-graph
               '((entry 0) (0 1) (1 2) (1 3) (2 7) (3 4) 
                 (3 5) (4 6) (5 6) (6 7) (7 1))))

  ;; Graph 6: looping diamond with backedges
  (define g6 (make-graph
               '((entry 1) (1 2) (1 3) (2 3) (3 4) (4 3) 
                 (4 5) (4 6) (5 7) (6 7) (7 4) (7 8) 
                 (8 9) (8 10) (9 1) (10 7))))

  ;; ---------------------------
  ;; get-dominators
  ;; ---------------------------
  (test-case "get-dominators: g1"
    (define golden (hash
                    'entry (set 'entry)
                    'a     (set 'entry 'a)
                    'b     (set 'entry 'a 'b)
                    'c     (set 'entry 'a 'b 'c)))
    (check-equal? (get-dominators g1) golden)
    (check-dominator-property g1 golden))

  (test-case "get-dominators: g2"
    (define golden (hash
                    'entry (set 'entry)
                    'a     (set 'entry 'a)
                    'b     (set 'entry 'a 'b)
                    'c     (set 'entry 'a 'c)
                    'd     (set 'entry 'a 'd)))
    (check-equal? (get-dominators g2) golden)
    (check-dominator-property g2 golden))

  (test-case "get-dominators: g3"
    (define golden
      (hash
       'entry (set 'entry)
       'a     (set 'entry 'a)
       'b     (set 'entry 'a 'b)
       'c     (set 'entry 'a 'b 'c)))
    (check-equal? (get-dominators g3) golden)
    (check-dominator-property g3 golden))

  (test-case "get-dominators: g4"
    (define golden
      (hash
       'entry (set 'entry)
       'a     (set 'entry 'a)
       'b     (set 'entry 'a 'b)
       'c     (set 'entry 'a 'b 'c)
       'd     (set 'entry 'a 'b 'd)
       'e     (set 'entry 'a 'b 'e)
       'f     (set 'entry 'a 'f)))
    (check-equal? (get-dominators g4) golden)
    (check-dominator-property g4 golden))

  (test-case "get-dominators: g5"
    (define golden
      (hash
       'entry (set 'entry)
       0      (set 'entry 0)
       1      (set 'entry 0 1)
       2      (set 'entry 0 1 2)
       3      (set 'entry 0 1 3)
       4      (set 'entry 0 1 3 4)
       5      (set 'entry 0 1 3 5)
       6      (set 'entry 0 1 3 6)
       7      (set 'entry 0 1 7)))
    (check-equal? (get-dominators g5) golden)
    (check-dominator-property g5 golden))

  (test-case "get-dominators: g6"
    (define golden
      (hash
       'entry (set 'entry)
       1      (set 'entry 1)
       2      (set 'entry 1 2)
       3      (set 'entry 1 3)
       4      (set 'entry 1 3 4)
       5      (set 'entry 1 3 4 5)
       6      (set 'entry 1 3 4 6)
       7      (set 'entry 1 3 4 7)
       8      (set 'entry 1 3 4 7 8)
       9      (set 'entry 1 3 4 7 8 9)
       10     (set 'entry 1 3 4 7 8 10)))
    (check-equal? (get-dominators g6) golden)
    (check-dominator-property g6 golden))

  ;; ---------------------------
  ;; get-dominator-tree
  ;; ---------------------------
  (test-case "get-dominator-tree: g1"
    (define golden (make-graph 
                     '((entry a) (a b) (b c))))
    (check-equal? (get-edges (get-dominator-tree g1)) (get-edges golden)))

  (test-case "get-dominator-tree: g2"
    (define golden (make-graph 
                     '((entry a) (a b) (a c) (a d))))
    (check-equal? (get-edges (get-dominator-tree g2)) (get-edges golden)))

  (test-case "get-dominator-tree: g3"
    (define golden (make-graph 
                     '((entry a) (a b) (b c))))
    (check-equal? (get-edges (get-dominator-tree g3)) (get-edges golden)))

  (test-case "get-dominator-tree: g4"
    (define golden (make-graph 
                     '((entry a) (a b) (a f) (b c) (b d) (b e))))
    (check-equal? (get-edges (get-dominator-tree g4)) (get-edges golden)))

  (test-case "get-dominator-tree: g5"
    (define golden (make-graph 
                     '((entry 0) (0 1) (1 2) (1 3) (1 7) (3 4) (3 5) (3 6))))
    (check-equal? (get-edges (get-dominator-tree g5)) (get-edges golden)))

  (test-case "get-dominator-tree: g6"
    (define golden (make-graph 
                     '((entry 1) (1 2) (1 3) (3 4) (4 5) (4 6) (4 7) (7 8) (8 9) (8 10))))
    (check-equal? (get-edges (get-dominator-tree g6)) (get-edges golden)))

  ;; ---------------------------
  ;; get-dominator-frontier
  ;; ---------------------------
  (test-case "get-dominator-frontier: g1"
    (define golden (hash
                    'entry (set)
                    'a     (set)
                    'b     (set)
                    'c     (set)))
    (check-equal? (get-dominator-frontier g1) golden))

  (test-case "get-dominator-frontier: g2"
    (define golden (hash
                    'entry (set)
                    'a     (set)
                    'b     (set 'd)
                    'c     (set 'd)
                    'd     (set)))
    (check-equal? (get-dominator-frontier g2) golden))

  (test-case "get-dominator-frontier: g3"
    (define golden (hash
                    'entry (set)
                    'a     (set 'a)
                    'b     (set 'a)
                    'c     (set)))
    (check-equal? (get-dominator-frontier g3) golden))

  (test-case "get-dominator-frontier: g4"
    (define golden (hash
                    'entry (set)
                    'a     (set)
                    'b     (set 'f)
                    'c     (set 'e)
                    'd     (set 'e)
                    'e     (set 'f)
                    'f     (set)))
    (check-equal? (get-dominator-frontier g4) golden))

  (test-case "get-dominator-frontier: g5"
    (define golden (hash
                    'entry (set)
                     0     (set)
                     1     (set 1)
                     2     (set 7)
                     3     (set 7)
                     4     (set 6)
                     5     (set 6)
                     6     (set 7)
                     7     (set 1)))
    (check-equal? (get-dominator-frontier g5) golden))

  (test-case "get-dominator-frontier: g6"
    (define golden (hash
                    'entry (set)
                     1     (set 1)
                     2     (set 3)
                     3     (set 1 3)
                     4     (set 1 3 4)
                     5     (set 7)
                     6     (set 7)
                     7     (set 1 4 7)
                     8     (set 1 7)
                     9     (set 1)
                     10    (set 7)))
    (check-equal? (get-dominator-frontier g6) golden))
  )
