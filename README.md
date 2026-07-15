# nix-lang

`nix-lang` is a parser, exact printer, and formatter for the Nix language built around a [Trees that Grow](https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/trees-that-grow-guidance) style AST. Inspired by [`ghc-exactprint`](https://hackage.haskell.org/package/ghc-exactprint), it supports lossless round-tripping for parsed source while preserving comments, formatting, and multiline structure after modifying the syntax tree. It also provides a fresh syntax tree for generated Nix code together with an RFC 166-oriented formatter for canonical output.

`nix-lang-qq` provides quasiquoter for building fresh Nix expressions in Haskell file and supports embedding Haskell expressions.

```haskell
formatExpr [nixQQ| { value = 1; } |]
  -- => {
  --      value = 1; 
  --    }
formatExpr [nixQQ| %%(foldl (\acc _ -> mkApp (mkVar "f") acc) (mkVar "x") [1..10 :: Int])]
  -- => f (f (f (f (f (f (f (f (f (f x)))))))))
```


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
