# nix-lang

`nix-lang` is a parser, exact printer, and formatter for the Nix language built around a [Trees that Grow](https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/trees-that-grow-guidance) style AST. Inspired by [`ghc-exactprint`](https://hackage.haskell.org/package/ghc-exactprint), it supports lossless round-tripping for parsed source while preserving comments, formatting, and multiline structure after modifying the syntax tree.

The library also provides a fresh syntax tree for generated Nix code together with an RFC 166-oriented formatter for canonical output.

## Overview

`nix-lang` currently provides:

* a parser for Nix expressions
* an exact printer for parsed syntax trees
* a fresh syntax tree for generated Nix expressions
* an RFC 166-oriented formatter for fresh syntax trees
* structured editing helpers for exact-print-preserving updates

## Known unsupported syntax

Although it now parses the entire nixpkgs, some valid syntax is not supported.

* Legacy let

```nix
let { x = 233; body = x; }
```

* Floating number starting with a decimal point

```nix
.233
```

* Chained has-attribute expressions

```nix
a ? b ? c ? d
```

* Operator without white spaces

```nix
1+-1
```
