#lang racket
(require 
  json 
  graph 
  racket/hash
  racket/pretty 
  racket/system
  racket/cmdline
  "lang-util.rkt"
  "expose-basic-blocks.rkt"
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

;; Parse command-line flag
(define ssa? #f)
(define licm? #f)

(command-line
 #:program "compiler"
 #:once-each
 [("--ssa") "Enable SSA transformation passes"
  (set! ssa? #t)]
 [("--licm") "Enable LICM (requires --ssa)"
  (set! licm? #t)])

(when (and licm? (not ssa?))
  (error "--licm requires --ssa"))

;; SSA-related passes 
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

;; Build the full pipeline with conditional SSA insertion
(define compile-pipeline
  (apply compose
         (append
          (list
            flatten-program
            bury-dead
            optimize-values)
          (if ssa? (if licm? ssa-licm-passes ssa-passes) '())
          (list
            remove-unused-instructions
            decorate-flow-graph
            expose-basic-blocks))))

;; Run compiler
(define program
  (compile-pipeline (read-json (current-input-port))))

(write-json program (current-output-port))
