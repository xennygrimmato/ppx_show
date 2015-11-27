let () = print_endline ([%show] ([1; 2], Some 42))


let () = Format.printf "%a@." [%pp] ([1; 2], Some 42)
