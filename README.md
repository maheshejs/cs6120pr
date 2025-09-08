# CS6120

## Layout

```
├── src/                  
│   ├── basic-blocks.rkt    # form basic blocks and construct CFG
│   ├── analyze-bril.rkt    # analyze Bril program and output instruction count histogram
├── benchmarks/             # benchmarks
│   └── core/
├── graphics/               # example CFGs
└── README.md
```

---

## Running

```bash
bril2json < <benchmark>.bril | racket src/<program>.rkt
```

---
