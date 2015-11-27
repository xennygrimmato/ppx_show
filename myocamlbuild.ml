(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2015, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *)

open Ocamlbuild_plugin

let () =
  dispatch (function
    | Before_options ->
      Options.use_ocamlfind := false

    | After_rules ->
      ocaml_lib "ocamlcommon" ~extern:true ~dir:"+compiler-libs";

      dep ["use_ppx_show"] ["src/ppx_show.byte"];

    | _ -> ())
