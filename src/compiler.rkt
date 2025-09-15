#lang racket
(require 
  json 
  graph 
  racket/hash
  racket/pretty 
  racket/system
  "lang-util.rkt"
  "expose-basic-blocks.rkt"
  "decorate-flow-graph.rkt"
  "remove-unused-instructions.rkt"
  "optimize-values.rkt"
  "undead-analysis.rkt"
  "bury-dead.rkt"
  "flatten-program.rkt")

;; Parse input program from stdin and compile
(define program (flatten-program
                  (bury-dead ;DCE
                    (optimize-values ;LVN
                      (remove-unused-instructions 
                        (decorate-flow-graph 
                          (expose-basic-blocks 
                            (read-json (current-input-port)))))))))

(write-json program (current-output-port))
