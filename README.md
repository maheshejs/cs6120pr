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
│   └── flatten-program.rkt             # JSON
├── test/                  
├── examples/                           # example programs
│   ├── basic-blocks.rkt                # form basic blocks and construct CFG 
│   └── analyze-bril.rkt                # analyze Bril program and output instruction count histogram
├── benchmarks/                         # benchmarks
│   └── core/
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
