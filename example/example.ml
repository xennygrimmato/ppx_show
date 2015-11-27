let () = print_endline ([%show] ([1; 2], Some 42))
let () = Format.printf "%a@." [%pp] ([1; 2], Some 42)

external show : _ -> string = "%show"
external pp : Format.formatter -> _ -> unit = "%pp"

let () = print_endline (show ([1; 2], Some 42))
let () = Format.printf "%a@." pp ([1; 2], Some 42)

module IntMap = Map.Make(struct type t = int let compare = compare end)

let map_of_list l = List.fold_left (fun acc (k, v) -> IntMap.add k v acc) IntMap.empty l

let () =
  Format.printf "%a@." [%pp]
    (map_of_list
      [ (1, "foo")
      ; (2, "bar")
      ; (42, "plop")
      ])
