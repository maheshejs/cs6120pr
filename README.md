# CS6120

## Layout

```
├── src/                  
│   ├── compiler.rkt                    # main compiler script
│   ├── <...>.rkt                       # compiler passes
│   ├── expose-basic-blocks.rkt         # BB
│   ├── decorate-flow-graph.rkt         # CFG
│   ├── remove-unused-instructions.rkt  # CFG
│   ├── optimize-values.rkt             # LVN 
│   ├── bury-dead.rkt                   # DCE
│   ├── undead-analysis.rkt             # DCE 
│   ├── dataflow-analysis.rkt           # DF
│   ├── dominator.rkt                   # DOM 
│   ├── fake-entry                      # SSA-PREP
│   ├── decorate-loops                  # SSA-PREP (natural loops)
│   ├── decorate-defines                # SSA-PREP
│   ├── insert-undefs                   # SSA-PREP
│   ├── place-phis                      # SSA-IN
│   ├── rename-variables                # SSA-IN
│   ├── taint-all-invariants            # SSA-IN (loop-invariant analysis)
│   ├── hoist-invariants                # SSA-IN (loop-invariant code motion)
│   ├── hoist-phis                      # SSA-IN (Pizlo's form)
│   ├── remove-phis                     # SSA-OUT
│   └── flatten-program.rkt             # JSON
├── test/                  
├── examples/                           # example programs
│   ├── basic-blocks.rkt                # form basic blocks and construct CFG 
│   ├── analyze-bril.rkt                # analyze Bril program and output instruction count histogram
│   └── llvm/Skeleton.cpp               # LLVM pass to transform `A and B` to `A xor B` xor `A or B` 
├── benchmarks/                         # Bril benchmarks
│   └── core/
├── embench/                            # C benchmarks
├── graphics/                           # example visualizations
├── results/                            # benchmark results
├── stats.py                            # stats script
└── README.md
```

---

## Running

```bash
bril2json < <test>.bril | racket src/compiler.rkt | bril2txt
```

---
