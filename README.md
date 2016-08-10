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

Building and Running
--------------------

1. Run `make` inside the `ppx_show` directory
2. This will create a `ppx_show.native` file.
3. Run: `ocamlc -ppx ./ppx_show.native example/example.ml`

How does it work?
-----------------

ppx\_show uses the OCaml typer to find the type inferred for the
argument of `show` or `pp` and generates a function according to this
type.

It has heuristics to print some abstract types such as sets and maps
obtained from `Set.Make` and `Map.Make`.

Tricks for abstract types
-------------------------

There are two ways to automatically print abstract types:

1. reveal them as private: `type t = private <internal representation>`
2. declare a specially named function that ppx\_show will recognize

For (2) the function must be named as follow:

- `string_of_<type-name>` for `[%show]`
- `pp_print_<type_name>` for `[%pp]`

The implementation can be as simple as:

```ocaml
let pp_print_t : _ -> t -> _ = [%pp]
```

Limitations
-----------

- only works with OCaml 4.03
- to use in code that requires `-rectypes`, you need to pass `-rectypes`
  to the ppx rewriter as well
