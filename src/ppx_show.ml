let () = Generators.linkme

let () =
  Ast_mapper.register "ppx_show" (fun _ ->
    { Ast_mapper.default_mapper with structure = fun _ s -> Gen_printer.map_impl s })

