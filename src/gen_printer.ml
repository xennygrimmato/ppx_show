(*
 * gen_printer.ml
 * --------------
 * Copyright : (c) 2011-2015, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
*)

open Primitive
open Asttypes
open Parsetree
open Ast_helper
open Typedtree
open Types
open Predef

type tuple_or_record =
  | Tuple  of Parsetree.expression list
  | Record of (string * Parsetree.expression) list

module type Loc = sig
  val loc : Location.t
end

module type Combinators = sig
  open Parsetree
  val typ : core_type -> core_type
  val get_param : Types.type_expr -> Types.type_expr
  val prefix : string
  val int : expression
  val char : expression
  val string : expression
  val float : expression
  val bool : expression
  val unit : expression
  val exn : expression
  val array : expression
  val list : expression
  val nativeint : expression
  val int32 : expression
  val int64 : expression
  val lazy_t : expression
  val var : expression
  val abstract : expression
  val arrow : expression
  val tuple : expression list -> expression
  val obj : expression
  val poly_variant : (string * expression option) list -> expression
  val package : expression
  val variant : (Longident.t * string * tuple_or_record) list -> expression
  val record : (Longident.t * string * expression) list -> expression
  val extension_constructor : expression
  val map : expression -> f:expression -> expression
end

module MakeHelpers(Loc : Loc) = struct
  open Loc

  let exp_unit =
    { pexp_desc = Pexp_construct ({ loc; txt = Lident "()" }, None);
      pexp_loc = loc;
      pexp_attributes = [] }

  let rec exp_seq l =
    match l with
    | [] ->
      failwith "Gen_printer.MakeHelpers.exp_seq"
    | [x] ->
      x
    | x :: l ->
      { pexp_desc = Pexp_sequence (x, exp_seq l);
        pexp_loc = loc;
        pexp_attributes = [] }

  let exp_ifthenelse e et ee =
    { pexp_desc = Pexp_ifthenelse (e, et, Some ee);
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_letrec l e =
    let l = List.map (fun (p, e) -> Vb.mk ~loc p e) l in
    { pexp_desc = Pexp_let (Recursive, l, e);
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_let p e e' =
    { pexp_desc = Pexp_let (Nonrecursive, [Vb.mk ~loc p e], e');
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_constraint e t =
    { pexp_desc = Pexp_constraint (e, t);
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_string str =
    { pexp_desc = Pexp_constant (Const_string (str, None));
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_int int =
    { pexp_desc = Pexp_constant (Const_int int);
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_fun pat exp =
    { pexp_desc = Pexp_fun (Nolabel, None, pat, exp);
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_function l =
    { pexp_desc = Pexp_function (List.map (fun (p, e) -> Exp.case p e) l);
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_apply exp args =
    { pexp_desc = Pexp_apply (exp, List.map (fun arg -> (Nolabel, arg)) args);
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_tuple = function
    | [x] ->
      x
    | l ->
      { pexp_desc = Pexp_tuple l;
        pexp_loc = loc;
        pexp_attributes = [] }

  let longident_of_list = function
    | [] -> failwith "Gen_printer.MakeHelpers.longident_of_list"
    | x :: l -> List.fold_left (fun acc x -> Longident.Ldot (acc, x)) (Longident.Lident x) l

  let exp_ident id =
    { pexp_desc = Pexp_ident { txt = longident_of_list id; loc };
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_var name =
    { pexp_desc = Pexp_ident { txt = Longident.Lident name; loc };
      pexp_loc = loc;
      pexp_attributes = [] }

  let exp_field e li =
    { pexp_desc = Pexp_field (e, { txt = li; loc });
      pexp_loc = loc;
      pexp_attributes = [] }

  let pat_array l =
    { ppat_desc = Ppat_array l;
      ppat_loc = loc;
      ppat_attributes = [] }

  let pat_lazy p =
    { ppat_desc = Ppat_lazy p;
      ppat_loc = loc;
      ppat_attributes = [] }

  let pat_any =
    { ppat_desc = Ppat_any;
      ppat_loc = loc;
      ppat_attributes = [] }

  let pat_construct li args =
    { ppat_desc = Ppat_construct ({ txt = li; loc }, args);
      ppat_loc = loc;
      ppat_attributes = [] }

  let pat_variant lbl arg =
    { ppat_desc = Ppat_variant (lbl, arg);
      ppat_loc = loc;
      ppat_attributes = [] }

  let pat_var name =
    { ppat_desc = Ppat_var { txt = name; loc };
      ppat_loc = loc;
      ppat_attributes = [] }

  let pat_tuple = function
    | [x] ->
      x
    | l ->
      { ppat_desc = Ppat_tuple l;
        ppat_loc = loc;
        ppat_attributes = [] }

  let pat_constraint p t =
    { ppat_desc = Ppat_constraint (p, t);
      ppat_loc = loc;
      ppat_attributes = [] }

  let typ_var v =
    { ptyp_desc = Ptyp_var v;
      ptyp_loc = loc;
      ptyp_attributes = [] }

  let typ_arrow a b =
    { ptyp_desc = Ptyp_arrow (Nolabel, a, b);
      ptyp_loc = loc;
      ptyp_attributes = [] }

  let typ_constr id args =
    { ptyp_desc = Ptyp_constr ({ txt = id; loc }, args);
      ptyp_loc = loc;
      ptyp_attributes = [] }

  let typ_poly vars t =
    match vars with
    | [] ->
      t
    | _ ->
      { ptyp_desc = Ptyp_poly (vars, t);
        ptyp_loc = loc;
        ptyp_attributes = [] }

  let gen_vars l =
    let rec map i l =
      match l with
      | [] -> []
      | _ :: l -> ("$" ^ string_of_int i) :: map (i + 1) l
    in
    map 0 l

  let rec longident_of_path = function
    | Path.Pident id -> Longident.Lident (Ident.name id)
    | Path.Pdot (a, b, _) -> Longident.Ldot (longident_of_path a, b)
    | Path.Papply (a, b) -> Longident.Lapply (longident_of_path a, longident_of_path b)
end

type ('a, 'b) either =
  | Inl of 'a
  | Inr of 'b

module PathMap : sig
  type 'a t
  val empty : 'a t
  val add : Path.t -> 'a -> 'a t -> 'a t
  val find : Path.t -> 'a t -> ('a, int) either
end = struct
  type 'a t = (Path.t * 'a) list
  let empty = []
  let add p x l = (p, x) :: l
  let find p l =
    let rec aux n l =
      match l with
      | [] -> Inr n
      | (p', x) :: l -> if Path.same p p' then Inl x else aux (n + 1) l
    in
    aux 0 l
end

module IntSet = Set.Make(struct type t = int let compare a b = a - b end)

module MakeGenerator(Loc : Loc)(Combinators : Combinators) = struct
  module Helpers = MakeHelpers(Loc)
  open Helpers

  let rec replace_last li name =
    match li with
    | Longident.Lident _ -> Longident.Lident name
    | Longident.Ldot (li, _) -> Longident.Ldot (li, name)
    | Longident.Lapply (li1, li2) -> Longident.Lapply (li1, replace_last li2 name)

  let rec path_last = function
    | Path.Pident id -> Ident.name id
    | Path.Pdot (_, id, _) -> id
    | Path.Papply (p1, p2) -> path_last p2

  let rec gen env printer_names printer_exprs params typ =
    match typ.desc with
    | Tvar _ ->
      (printer_names,
       printer_exprs,
       if IntSet.mem typ.id params then
         exp_var ("$gen" ^ string_of_int typ.id)
       else
         Combinators.var)
    | Tarrow _ ->
      (printer_names,
       printer_exprs,
       Combinators.arrow)
    | Ttuple l ->
      let printer_names, printer_exprs, printers = gen_printers env printer_names printer_exprs params l in
      (printer_names,
       printer_exprs,
       Combinators.tuple printers)
    | Tconstr (path, args, _) -> begin
        match PathMap.find path printer_names with
        | Inl name -> begin
            match args with
            | [] ->
              (printer_names,
               printer_exprs,
               exp_var name)
            | _ ->
              let printer_names, printer_exprs, printers = gen_printers env printer_names printer_exprs params args in
              (printer_names,
               printer_exprs,
               exp_fun (pat_var "$") (exp_apply (exp_var name) (printers @ [exp_var "$"])))
          end
        | Inr n ->
          let name = "$gen_aux" ^ string_of_int n ^ "=" ^ Path.name path in
          let printer_names = PathMap.add path name printer_names in
          let printer_names, printer_exprs, printer = gen_constr_printer env printer_names printer_exprs path in
          let vars = gen_vars args in
          let typ =
            typ_poly
              vars
              (List.fold_right
                 (fun var typ -> typ_arrow (Combinators.typ (typ_var var)) typ)
                 vars
                 (Combinators.typ (typ_constr (longident_of_path path) (List.map typ_var vars))))
          in
          let printer_exprs = (name, typ, printer) :: printer_exprs in
          let printer_names, printer_exprs, printers = gen_printers env printer_names printer_exprs params args in
          match args with
          | [] ->
            (printer_names,
             printer_exprs,
             exp_var name)
          | _ ->
            (printer_names,
             printer_exprs,
             exp_fun (pat_var "$") (exp_apply (exp_var name) (printers @ [exp_var "$"])))
      end
    | Tobject _ ->
      (printer_names,
       printer_exprs,
       Combinators.obj)
    | Tfield _ ->
      assert false
    | Tnil ->
      assert false
    | Tlink typ ->
      gen env printer_names printer_exprs params typ
    | Tsubst typ ->
      gen env printer_names printer_exprs params typ
    | Tvariant { row_fields = l } ->
      let rec aux printer_names printer_exprs l =
        match l with
        | [] ->
          (printer_names, printer_exprs, [])
        | (name, field) :: l ->
          let printer_names, printer_exprs, l = aux printer_names printer_exprs l in
          match field with
          | Rpresent None ->
            (printer_names,
             printer_exprs,
             (name, None) :: l)
          | Rpresent (Some typ) ->
            let printer_names, printer_exprs, printer = gen env printer_names printer_exprs params typ in
            (printer_names,
             printer_exprs,
             (name, Some printer) :: l)
          | _ ->
            (printer_names, printer_exprs, l)
      in
      let printer_names, printer_exprs, l = aux printer_names printer_exprs l in
      (printer_names,
       printer_exprs,
       Combinators.poly_variant l)
    | Tunivar _ ->
      (printer_names,
       printer_exprs,
       Combinators.var)
    | Tpoly (typ, _) ->
      gen env printer_names printer_exprs params typ
    | Tpackage _ ->
      (printer_names,
       printer_exprs,
       Combinators.package)

  and gen_printers env printer_names printer_exprs params typs =
    match typs with
    | [] ->
      (printer_names, printer_exprs, [])
    | typ :: typs ->
      let printer_names, printer_exprs, p = gen env printer_names printer_exprs params typ in
      let printer_names, printer_exprs, l = gen_printers env printer_names printer_exprs params typs in
      (printer_names, printer_exprs, p :: l)

  and gen_constr_printer env printer_names printer_exprs path =
    let li = replace_last (longident_of_path path) (Combinators.prefix ^ path_last path) in
    if try let _ = Env.lookup_value li env in true with Not_found -> false then
      (printer_names,
       printer_exprs,
       { pexp_desc = Pexp_ident { txt = li; loc = Loc.loc };
         pexp_loc = Loc.loc;
         pexp_attributes = [] })
    else if Path.same path path_int then
      (printer_names,
       printer_exprs,
       Combinators.int)
    else if Path.same path path_char then
      (printer_names,
       printer_exprs,
       Combinators.char)
    else if Path.same path path_string then
      (printer_names,
       printer_exprs,
       Combinators.string)
    else if Path.same path path_float then
      (printer_names,
       printer_exprs,
       Combinators.float)
    else if Path.same path path_bool then
      (printer_names,
       printer_exprs,
       Combinators.bool)
    else if Path.same path path_unit then
      (printer_names,
       printer_exprs,
       Combinators.unit)
    else if Path.same path path_exn then
      (printer_names,
       printer_exprs,
       Combinators.exn)
    else if Path.same path path_nativeint then
      (printer_names,
       printer_exprs,
       Combinators.nativeint)
    else if Path.same path path_int32 then
      (printer_names,
       printer_exprs,
       Combinators.int32)
    else if Path.same path path_int64 then
      (printer_names,
       printer_exprs,
       Combinators.int64)
    else if Path.same path path_lazy_t then
      (printer_names,
       printer_exprs,
       Combinators.lazy_t)
    else if path = path_list then
      (printer_names,
       printer_exprs,
       Combinators.list)
    else if path = path_array then
      (printer_names,
       printer_exprs,
       Combinators.array)
    else
      let li = longident_of_path path in
      match try Some (Env.find_type path env) with Not_found -> None with
      | None ->
        (printer_names,
         printer_exprs,
         Combinators.abstract)
      | Some decl ->
        let mkfun e =
          let rec aux = function
            | [] ->
              e
            | { id } :: params ->
              exp_fun (pat_var ("$gen" ^ string_of_int id)) (aux params)
          in
          aux decl.type_params
        in
        let params =
          List.fold_left (fun set typ -> IntSet.add typ.id set)
            IntSet.empty decl.type_params
        in
        match decl with
        | { type_kind = Type_variant l } ->
          let rec aux printer_names printer_exprs l =
            match l with
            | [] ->
              (printer_names, printer_exprs, [])
            |  { cd_id = name; cd_args = args } :: l ->
              let printer_names, printer_exprs, l =
                aux printer_names printer_exprs l
              in
              let printer_names, printer_exprs, printers =
                match args with
                | Cstr_tuple args ->
                  let printer_names, printer_exprs, printers =
                    gen_printers env printer_names printer_exprs params args
                  in
                  (printer_names, printer_exprs, Tuple printers)
                | Cstr_record lds ->
                  let printer_names, printer_exprs, printers =
                    gen_printers env printer_names printer_exprs params
                      (List.map (fun ld -> ld.ld_type) lds)
                  in
                  (printer_names, printer_exprs,
                   Record
                     (List.map2 (fun {ld_id;_} e -> (Ident.name ld_id, e)) lds printers))
              in
              let name = Ident.name name in
              (printer_names,
               printer_exprs,
               (replace_last li name, name, printers) :: l)
          in
          let printer_names, printer_exprs, l =
            aux printer_names printer_exprs l
          in
          (printer_names,
           printer_exprs,
           mkfun (Combinators.variant l))
        | { type_kind = Type_record (l, _) } ->
          let rec aux printer_names printer_exprs l =
            match l with
            | [] ->
              (printer_names, printer_exprs, [])
            | { ld_id = id; ld_type = typ; _ } :: l ->
              let name = Ident.name id in
              let printer_names, printer_exprs, printer = gen env printer_names printer_exprs params typ in
              let printer_names, printer_exprs, l = aux printer_names printer_exprs l in
              (printer_names,
               printer_exprs,
               (replace_last li name, name, printer) :: l)
          in
          let printer_names, printer_exprs, l = aux printer_names printer_exprs l in
          (printer_names,
           printer_exprs,
           mkfun (Combinators.record l))
        | { type_kind = Type_abstract; type_manifest = Some typ; type_private = Public } ->
          let printer_names, printer_exprs, printer = gen env printer_names printer_exprs params typ in
          (printer_names,
           printer_exprs,
           mkfun printer)
        | { type_kind = Type_abstract; type_manifest = Some typ; type_private = Private } ->
          let printer_names, printer_exprs, printer = gen env printer_names printer_exprs params typ in
          (printer_names,
           printer_exprs,
           (* We need to do a coercion here. *)
           mkfun (exp_apply (exp_ident ["Obj"; "magic"]) [printer]))
        | { type_kind = Type_abstract; type_manifest = None } ->
          let li = longident_of_path path in
          let bindings = replace_last li "bindings" in
          let elements = replace_last li "elements" in
          let has li = try let _ = Env.lookup_value li env in true with Not_found -> false in
          begin match has bindings, has elements, decl.type_params with
          | true, _, [x] ->
            let key_path, _ = Env.lookup_type (replace_last li "key") env in
            let printer_names, printer_exprs, print_key =
              gen_constr_printer env printer_names printer_exprs key_path
            in
            let printer_names, printer_exprs, print_val =
              gen env printer_names printer_exprs params x
            in
            let printer_names, printer_exprs, print_list =
              gen_constr_printer env printer_names printer_exprs path_list
            in
            (printer_names, printer_exprs,
             mkfun (Combinators.map
                      (exp_apply print_list [Combinators.tuple [print_key; print_val]])
                      ~f:(let loc = Loc.loc in Exp.ident ~loc { txt = bindings; loc })))
          | _, true, [] ->
            let elt_path, _ = Env.lookup_type (replace_last li "elt") env in
            let printer_names, printer_exprs, print_elt =
              gen_constr_printer env printer_names printer_exprs elt_path
            in
            let printer_names, printer_exprs, print_list =
              gen_constr_printer env printer_names printer_exprs path_list
            in
            (printer_names, printer_exprs,
             mkfun (Combinators.map
                      (exp_apply print_list [Combinators.tuple [print_elt]])
                      ~f:(let loc = Loc.loc in Exp.ident ~loc { txt = elements; loc })))
          | _ ->
            (printer_names,
             printer_exprs,
             mkfun Combinators.abstract)
          end
        | { type_kind = Type_open } ->
          (printer_names,
           printer_exprs,
           Combinators.extension_constructor)

  let rec generate env typ =
    let printer_names, printer_exprs, printer = gen env PathMap.empty [] IntSet.empty (Combinators.get_param typ) in
    match printer_exprs with
    | [] ->
      printer
    | _ ->
      exp_letrec
        (List.map
           (fun (name, typ, expr) ->
              let expr =
                match expr.pexp_desc with
                | Pexp_function _ ->
                  expr
                | _ ->
                  exp_fun (pat_var "$") (exp_apply expr [exp_var "$"])
              in
              (pat_constraint (pat_var name) typ, expr))
           printer_exprs)
        printer
end

module GeneratorOfCombinators(Make : functor (Loc : Loc) -> Combinators) = struct
  let generate env typ loc =
    let module Loc = struct let loc = loc end in
    let module Gen = MakeGenerator(Loc)(Make(Loc)) in
    Gen.generate env typ
end

let generators_by_ext_name = Hashtbl.create 8
let generators_by_prim_name = Hashtbl.create 8
let prim_name s = "%" ^ s
let register_generator name ~generate ~combinator_type =
  Hashtbl.add generators_by_ext_name name combinator_type;
  Hashtbl.add generators_by_prim_name (prim_name name) generate

let second_pass =
  let super = Untypeast.default_mapper in
  let expr this expr =
    match expr.exp_desc with
    | Texp_ident (_, _, { val_kind = Val_prim { prim_name = name } }) -> begin
        match Hashtbl.find generators_by_prim_name name with
        | f -> f expr.exp_env expr.exp_type expr.exp_loc
        | exception Not_found -> super.expr this expr
      end
    | _ -> super.expr this expr
  in
  { super with expr }

let fake_primitive ~loc prim typ =
  Exp.letmodule ~loc { txt = "M"; loc }
    (Mod.structure ~loc
       [ Str.primitive ~loc
           (Val.mk ~loc { txt = "f"; loc }
              typ
              ~prim:[prim])
       ])
    (Exp.ident ~loc { txt = Ldot (Lident "M", "f"); loc })

let first_pass =
  let super = Ast_mapper.default_mapper in
  let expr this expr =
    match expr.pexp_desc with
    | Pexp_extension ({ txt = name; loc }, PStr []) -> begin
        match Hashtbl.find generators_by_ext_name name with
        | f ->
          fake_primitive ~loc (prim_name name) (f (Typ.var ~loc "a"))
        | exception Not_found -> super.expr this expr
      end
    | _ -> super.expr this expr
  in
  { super with expr }

let map_impl impl =
  match impl with
  | [] -> []
  | { pstr_loc = loc; _ } :: _ ->
    let impl = first_pass.structure first_pass impl in

    let fname = loc.loc_start.pos_fname in
    let opref = Compenv.output_prefix fname in
    let module_name = Compenv.module_of_filename Format.err_formatter fname opref in
    Compmisc.init_path false;
    Env.set_unit_name module_name;
    let env = Compmisc.initial_env () in
    let timpl, _ = Typemod.type_implementation fname opref module_name env impl in

    second_pass.structure second_pass timpl
