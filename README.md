# CS6120

## Layout

```
├── src/                  
│   ├── compiler.rkt                # main script
│   ├── <...>.rkt                   # compiler passes
│   ├── expose-basic-blocks.rkt     # BB
│   ├── decorate-flow-graph.rkt     # CFG
│   ├── optimize-values.rkt         # LVN 
│   └── bury-dead.rkt               # DCE
├── test/                  
├── examples/                       # example programs
│   ├── basic-blocks.rkt            # form basic blocks and construct CFG 
│   └── analyze-bril.rkt            # analyze Bril program and output instruction count histogram
├── benchmarks/                     # benchmarks
│   └── core/
├── graphics/                       # example CFGs
└── README.md
```

---

## Running

```bash
bril2json < <test>.bril | racket src/compiler.rkt | bril2txt
```

---
