let () = Generators.linkme

let () =
  Ast_mapper.register "ppx_show" (fun args ->
    if List.mem "-rectypes" args then Clflags.recursive_types := true;
    { Ast_mapper.default_mapper with structure = fun _ s -> Gen_printer.map_impl s })

