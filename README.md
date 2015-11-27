Generic printer for OCaml values
================================

This ppx rewriters add support for the `[%show]` and `[%pp]` extension
points which do exactly what you expect:

```ocaml
$ [%show] (1, 2, Some 'c');;
- : string = "(1, 2, Some 'c')"
$ Format.printf "%a@." [%pp] (1, 2, Some 'c');;
(1, 2, Some 'c')
- : unit = ()
```

And to save a few characters you can also do this:

```ocaml
external show : _ -> string = "%show"
external pp : Format.formatter -> _ -> unit = "%pp"
```

The code is based on a old compiler path available
[here](https://github.com/diml/ocaml-3.12.1-print).

I wouldn't recommend using this in production code, but it is really
useful for quick debugging.

Limitations
-----------

- only works with OCaml 4.03
- to use in code that requires `-rectypes`, you need to pass `-rectypes`
  to the ppx rewriter as well
