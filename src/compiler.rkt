#lang racket
(require 
  json
  graph
  racket/hash
  racket/pretty
  racket/system
  racket/cmdline
  racket/list
  "lang-util.rkt"
  "expose-basic-blocks.rkt"
  "expose-speculated-calls.rkt"
  "trace-inline-calls.rkt"
  "decorate-flow-graph.rkt"
  "remove-unused-instructions.rkt"
  "fake-entry.rkt"
  "decorate-loops.rkt"
  "decorate-defines.rkt"
  "insert-undefs.rkt"
  "place-phis.rkt"
  "rename-variables.rkt"
  "taint-invariants.rkt"
  "taint-all-invariants.rkt"
  "hoist-invariants.rkt"
  "hoist-phis.rkt"
  "remove-phis.rkt"
  "optimize-values.rkt"
  "undead-analysis.rkt"
  "bury-dead.rkt"
  "flatten-program.rkt")

;; Flags and values
(define ssa? #f)
(define licm? #f)
(define spec? #f)
(define spec-file #f)

;; Parse command-line
(command-line
 #:program "compiler"
 #:once-each
 [("--ssa") "Enable SSA transformation passes"
  (set! ssa? #t)]
 [("--licm") "Enable LICM (requires --ssa)"
  (set! licm? #t)]
 [("--spec") file "Enable trace inlining and provide spec filename"
  (set! spec? #t)
  (set! spec-file file)])

;; Validate flag dependencies
(when (and licm? (not ssa?))
  (error "--licm requires --ssa"))

(when (and spec? (not spec-file))
  (error "--spec requires a filename argument"))

;; ==== Pass lists ====
(define ssa-passes
  (list
    remove-phis
    hoist-phis
    rename-variables
    place-phis
    insert-undefs
    decorate-defines
    decorate-loops
    fake-entry))

(define ssa-licm-passes
  (list
    remove-phis
    hoist-phis
    hoist-invariants
    taint-all-invariants
    rename-variables
    place-phis
    insert-undefs
    decorate-defines
    decorate-loops
    fake-entry))

;; Passes that are spec-aware (take (prog spec-data))
(define spec-passes
  (list
    trace-inline-calls        
    expose-speculated-calls))

;; Build the ordered list of all passes depending on flags
(define all-passes
  (append
    (list
      flatten-program
      bury-dead
      optimize-values
      )
    (if ssa? (if licm? ssa-licm-passes ssa-passes) '())
    (list
      remove-unused-instructions
      decorate-flow-graph)
    (if spec? spec-passes (list expose-basic-blocks))))

;; Wrap passes so each wrapper accepts (prog spec-data) uniformly.
;; If the pass is in `spec-passes` we call it with two args; otherwise with one.
(define pass-wrappers
  (map (lambda (pass)
         (if (memq pass spec-passes)
             ;; pass expects (prog spec-data)
             (lambda (prog spec-data) (pass prog spec-data))
             ;; pass expects (prog)
             (lambda (prog spec-data) (pass prog))))
       all-passes))

;; Run the pipeline threading spec-data to the binary passes
(define (run-passes initial-prog spec-data)
  (foldr (lambda (pass prog) (pass prog spec-data))
         initial-prog
         pass-wrappers))

;; ==== IO: read stdin program and spec-file (if any) ====
(define program-json
  (read-json (current-input-port)))

(define spec-data
  (if spec-file
      (call-with-input-file spec-file read-json)
      #f))

;; Run compile pipeline
(define program
  (run-passes program-json spec-data))

;; Output resulting program to stdout
(write-json program (current-output-port))
