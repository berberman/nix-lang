# nix-lang

A toy parser and exact printer for Nix language that produces and operates on [Trees that Grow](https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/trees-that-grow-guidance) style annotated AST. Inspired by [`ghc-exactprint`](https://hackage.haskell.org/package/ghc-exactprint), it supports lossless round-tripping while preserving comments, formatting , and multiline structure after modifying the syntax tree.

## Known unsupported syntaxes

Although it now parses the entire nixpkgs, some valid syntaxes are not supported.

* Legacy let

```nix
let { x = 233; body = x; }
```

* Floating number started with decimal point

```nix
.233
```

* Multi has attr

```nix
a ? b ? c ? d
```

* Operator without white spaces

```nix
1+-1
```
