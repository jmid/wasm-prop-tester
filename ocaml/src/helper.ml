open Wasm
open QCheck

type context_ = {
          (* (input, result) list *)
  (*labels: (Types.value_type option * Types.stack_type) list;*)
  labels: (Types.value_type option * Types.value_type option) list;
  funcs: (Types.stack_type * Types.value_type option) list;
  tables: (Types.value_type option * Types.stack_type) list;
  locals: Types.stack_type;
  globals: Types.stack_type;
  mems: Types.memory_type list;
  return: Types.value_type option;
  funcindex: int;
}

let string_to_name s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (Char.code(s.[i]) :: l) in
  exp (String.length s - 1) []
;;

let as_phrase x = {Source.at = Source.no_region; Source.it = x}
;;

let get_module types funcs = {
  Ast.types = types;
  Ast.globals = [];
  Ast.tables = [];
  Ast.memories = [];
  Ast.funcs = funcs;
  Ast.start = None;
  Ast.elems  = [];
  Ast.data = [];
  Ast.imports = [];
  Ast.exports = [
    as_phrase ({
      Ast.name = string_to_name "aexp";
      Ast.edesc = as_phrase (Ast.FuncExport (as_phrase 0l));
    })
  ];
}
;;

let types_ = [ 
  as_phrase (Types.FuncType ([], []));
  as_phrase (Types.FuncType ([], [Types.I32Type]));
  as_phrase (Types.FuncType ([], [Types.I64Type]));
  as_phrase (Types.FuncType ([], [Types.F32Type]));
  as_phrase (Types.FuncType ([], [Types.F64Type]))
];;

(** get_indexes: a' -> a list -> int list **)
let get_indexes a l =
  let rec get_index a' l' i = match List.nth_opt l' i with
    | Some e  -> if a' = e then i::(get_index a' l' (i+1)) else get_index a' l' (i+1)
    | None    -> [] in
  get_index a l 0

(** get_random_element: 'a -> ('a * 'b) list -> 'b **)
let get_random_element a l =
  let rec get_index a' l' i = match List.nth_opt l' i with
    | Some (e, t)  -> if a' = t then e::(get_index a' l' (i+1)) else get_index a' l' (i+1)
    | None       -> [] in
  let type_list = get_index a l 0 in
  match List.length type_list with
    | 0 -> None
    | n -> Some (List.nth type_list (Random.int n))

(** get_indexes_and_inputs: a' -> a list -> (int * stack_type) list **)
let get_indexes_and_inputs a l =
  let rec get_index_ot a' b' l' i = match List.nth_opt l' i with
    | Some (e, t)  -> if a' = t && b' = e then (i,t)::(get_index_ot a' b' l' (i+1)) else get_index_ot a' b' l' (i+1)
    | None       -> [] in
  match get_random_element a l with
    | Some t -> get_index_ot a t l 0
    | None   -> []


(** get_random_element: 'a -> ('a * 'b) list -> 'b **)
let get_random_element2 a l index =
  let rec get_index a' l' i = match i = index with
    | true  -> get_index a' l' (i+1)
    | false -> match List.nth_opt l' i with
      | Some (e, t)  -> if a' = t then e::(get_index a' l' (i+1)) else get_index a' l' (i+1)
      | None       -> [] in
  let type_list = get_index a l 0 in
  match List.length type_list with
    | 0 -> None
    | n -> Some (List.nth type_list (Random.int n))

(** get_indexes_and_inputs: a' -> a list -> (int * stack_type) list **)
let get_indexes_and_inputs2 a l index =
  let rec get_index_ot a' b' l' i = match List.nth_opt l' i with
    | Some (e, t)  -> if a' = t && b' = e then (i,e)::(get_index_ot a' b' l' (i+1)) else get_index_ot a' b' l' (i+1)
    | None       -> [] in
  match get_random_element2 a l index with
    | Some t -> get_index_ot a t l 0
    | None   -> []


(* 
(** get_indexes_and_inputs: a' -> a list -> (int * stack_type) list **)
let get_indexes_and_inputs2 a l =
  let ot = match a with
    | Some e -> [e]
    | None   -> [] in 
    let rec get_index_ot a' b' l' i = match List.nth_opt l' i with
      | Some (e, t)  -> if a' = t && b' = e then (i,t)::(get_index_ot a' b' l' (i+1)) else get_index_ot a' b' l' (i+1)
      | None       -> [] in
    match get_random_element a l with
      | Some t -> get_index_ot a t l 0
      | None   -> []*)