open Ocamlbuild_plugin

let () =
  dispatch (function
    | Before_options ->
      Options.use_ocamlfind := false

    | After_rules ->
      ocaml_lib "ocamlcommon" ~extern:true ~dir:"+compiler-libs"

    | _ -> ())
