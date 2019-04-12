open Wasm
open QCheck

type globals_ = {
  i32: (int * Types.mutability) list;
  i64: (int * Types.mutability) list;
  f32: (int * Types.mutability) list;
  f64: (int * Types.mutability) list;
}

type value_type = I32Type | I64Type | F32Type | F64Type | IndexType

type context_ = {
          (* (input, result) list *)
  (*labels: (Types.value_type option * Types.stack_type) list;*)
  labels: (value_type option * value_type option) list;
  funcs: (value_type list * value_type option) list;
  imports: (value_type list * value_type option) list;
  (* tables: (value_type option * Types.stack_type) list; *)
  mems: Int32.t Types.limits option;
  data: string Ast.segment list;
  tables: Int32.t Types.limits option;
  elems: Ast.var list Ast.segment list;
  locals: value_type list;
  globals: globals_;
  return: value_type option;
  funcindex: int;
}

let to_wasm_value_type = function
  | I32Type   -> Types.I32Type
  | I64Type   -> Types.I64Type
  | F32Type   -> Types.F32Type
  | F64Type   -> Types.F64Type
  | IndexType -> Types.I32Type

let rec to_stack_type = function
  | []     -> []
  | e::rst -> (to_wasm_value_type e)::(to_stack_type rst)

let string_to_name s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (Char.code(s.[i]) :: l) in
  exp (String.length s - 1) []
;;

let as_phrase x = {Source.at = Source.no_region; Source.it = x}
;;

let get_module types funcs memories globals data tables elems = {
  Ast.types = types;
  Ast.globals = globals;
  Ast.tables = tables;
  Ast.memories = memories;
  Ast.funcs = funcs;
  Ast.start = None(*None*)(*Some (as_phrase 1l)*);
  Ast.elems  = elems;
  Ast.data = data;
  Ast.imports = [
    as_phrase({
      Ast.module_name = string_to_name "imports";
      Ast.item_name = string_to_name "log";
      Ast.idesc = as_phrase (Ast.FuncImport (as_phrase 0l))
    })
  ];
  Ast.exports = [
    as_phrase ({
      Ast.name = string_to_name "aexp";
      Ast.edesc = as_phrase (Ast.FuncExport (as_phrase 2l));
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
  let rec get_index a' l' i = try match (List.nth l' i) = a' with
    | true  -> i::(get_index a' l' (i+1)) 
    | false -> get_index a' l' (i+1) with Failure _ -> [] in
  get_index a l 0

let typeToString a = Types.string_of_value_type a

let mToString = function
  | Some m -> (match m with 
    | Types.Mutable   -> "Mutable"
    | Types.Immutable -> "Immutable")
  | None   -> "None"

(** get_global_indexes: a' -> a list -> int list **)
let get_global_indexes t (l: globals_) m =
  match t with 
    | I32Type -> List.fold_left 
      (fun l (i, m') -> 
        (match m with
          | Some n ->  
            if n = m' then i::l else l 
          | None   -> i::l)) [] l.i32
    | I64Type -> List.fold_left 
      (fun l (i, m') -> 
        (match m with
          | Some n ->  
            if n = m' then i::l else l 
          | None   -> i::l)) [] l.i64
    | F32Type -> List.fold_left 
      (fun l (i, m') -> 
        (match m with
          | Some n ->  
            if n = m' then i::l else l 
          | None   -> i::l)) [] l.f32
    | F64Type -> List.fold_left 
      (fun l (i, m') -> 
        (match m with
          | Some n ->  
            if n = m' then i::l else l 
          | None   -> i::l)) [] l.f64
    | IndexType -> []


(** get_random_element: 'a -> ('a * 'b) list -> 'b **)
let get_random_element a l =
  let rec get_index a' l' i = try
    let element = List.nth l' i in
    match snd element = a' with
      | true  -> (fst element)::(get_index a' l' (i+1))
      | false -> get_index a' l' (i+1) with Failure _ -> [] in
  let type_list = get_index a l 0 in
  match List.length type_list with
    | 0 -> None
    | n -> try Some (List.nth type_list (Random.int n)) with Failure _ -> None

(** get_indexes_and_inputs: a' -> a list -> (int * stack_type) list **)
let get_indexes_and_inputs a l =
  let rec get_index_ot a' b' l' i = try
    let element = List.nth l' i in
    match snd element = a' && fst element = b' with
      | true  -> (i,(snd element))::(get_index_ot a' b' l' (i+1))
      | false -> get_index_ot a' b' l' (i+1) with Failure _ -> [] in
  match get_random_element a l with
    | Some t -> get_index_ot a t l 0
    | None   -> []


(** get_random_global: 'a -> ('a * 'b) list -> int -> 'b **)
let get_random_global a l index =
  let rec get_index a' l' i = 
    match i = index with
      | true  -> get_index a' l' (i+1)
      | false -> (try
        let element = List.nth l' i in
        match snd element = a' with
          | true  -> (i,(fst element))::(get_index a' l' (i+1))
          | false -> get_index a' l' (i+1) with Failure _ -> []) in
  let type_list = get_index a l 0 in
  match List.length type_list with
    | 0 -> None
    | n -> try Some (List.nth type_list (Random.int n)) with Failure _ -> None     

(*
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
*)

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