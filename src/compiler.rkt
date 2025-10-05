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
  "decorate-defines.rkt"
  "insert-undefs.rkt"
  "place-phis.rkt"
  "rename-variables.rkt"
  "hoist-phis.rkt"
  "remove-phis.rkt"
  "optimize-values.rkt"
  "undead-analysis.rkt"
  "bury-dead.rkt"
  "flatten-program.rkt")

;; Parse command-line flag
(define ssa? #f)
(command-line
 #:program "compiler"
 #:once-each
 [("--ssa") "Enable SSA transformation passes"
  (set! ssa? #t)])

;; SSA-related passes 
(define ssa-passes
  (list
    remove-phis
    hoist-phis
    rename-variables
    place-phis
    insert-undefs
    decorate-defines
    fake-entry))

;; Build the full pipeline with conditional SSA insertion
(define compile-pipeline
  (apply compose
         (append
          (list
            flatten-program
            bury-dead
            optimize-values)
          (if ssa? ssa-passes '())
          (list
            remove-unused-instructions
            decorate-flow-graph
            expose-basic-blocks))))

;; Run compiler
(define program
  (compile-pipeline (read-json (current-input-port))))

(write-json program (current-output-port))
