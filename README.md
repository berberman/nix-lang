# nix-lang

A toy parser for Nix language that produces "Trees that Grow" style annotated AST.

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
