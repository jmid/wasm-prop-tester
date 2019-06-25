open Wasm
open QCheck

type globals_ = {
  g_m_i32:  int list;
  g_im_i32: int list;
  g_m_i64:  int list;
  g_im_i64: int list;
  g_m_f32:  int list;
  g_im_f32: int list;
  g_m_f64:  int list;
  g_im_f64: int list;
}

type value_type = I32Type | I64Type | F32Type | F64Type | MemIndexType | TableIndex of int

type funcs_ = {
  f_none: (int * value_type list) list;
  f_i32:  (int * value_type list) list;
  f_i64:  (int * value_type list) list;
  f_f32:  (int * value_type list) list;
  f_f64:  (int * value_type list) list;
}

type context_ = {
  labels:  (value_type option * value_type option) list;
  funcs:   funcs_;
  imports: (value_type list * value_type option) list;
  mems:    Int32.t Types.limits option;
  data:    string Ast.segment list;
  tables:  Int32.t Types.limits option;
  elems:   ((value_type option * int) option) array option;
  locals:  value_type list;
  globals: globals_;
  return:  value_type option;
  funcindex: int;
}

let to_wasm_value_type = function
  | I32Type      -> Types.I32Type
  | I64Type      -> Types.I64Type
  | F32Type      -> Types.F32Type
  | F64Type      -> Types.F64Type
  | MemIndexType -> Types.I32Type
  | TableIndex _ -> Types.I32Type

let rec to_stack_type = function
  | []     -> []
  | e::rst -> (to_wasm_value_type e)::(to_stack_type rst)

let string_to_name s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (Char.code(s.[i]) :: l) in
  exp (String.length s - 1) []
;;

let as_phrase x = { Source.at = Source.no_region; Source.it = x }
;;

let types_ = [ 
  as_phrase (Types.FuncType ([], []));
  as_phrase (Types.FuncType ([], [Types.I32Type]));
  as_phrase (Types.FuncType ([], [Types.I64Type]));
  as_phrase (Types.FuncType ([], [Types.F32Type]));
  as_phrase (Types.FuncType ([], [Types.F64Type]))
];;


(** get_indexes: 'a -> 'a list -> int list **)
let get_indexes a l =
  let rec get_index a' l' i = match l' with
    | [] -> []
    | e::es ->
      if e = a'
      then i::(get_index a' es (i+1))
      else (get_index a' es (i+1)) in
  get_index a l 0

let typeToString = Types.string_of_value_type

let mToString = function
  | None   -> "None"
  | Some m -> (match m with 
    | Types.Mutable   -> "Mutable"
    | Types.Immutable -> "Immutable")

(** get_global_indexes: a' -> a list -> int list **)
let get_global_indexes t (l: globals_) m = match t with 
  | MemIndexType -> []
  | TableIndex _ -> []
  | I32Type -> (match m with
      | Some Types.Mutable   -> l.g_m_i32
      | Some Types.Immutable -> l.g_im_i32
      | None                 -> l.g_im_i32)
  | I64Type -> (match m with
      | Some Types.Mutable   -> l.g_m_i64
      | Some Types.Immutable -> l.g_im_i64
      | None                 -> l.g_im_i64)
  | F32Type -> (match m with
      | Some Types.Mutable   -> l.g_m_f32
      | Some Types.Immutable -> l.g_im_f32
      | None                 -> l.g_im_f32)
  | F64Type ->  (match m with
      | Some Types.Mutable   -> l.g_m_f64
      | Some Types.Immutable -> l.g_im_f64
      | None                 -> l.g_im_f64)

(** get_random_element: 'a -> ('b * 'a) list -> 'b option **)
let get_random_element a l =
  let rec get_index a' l' = match l' with
    | [] -> []
    | (fst,snd)::es ->
      if snd = a'
      then fst::(get_index a' es)
      else get_index a' es in
  let type_list = get_index a l in
  match List.length type_list with
    | 0 -> None
    | n -> try Some (List.nth type_list (Random.int n)) with Failure _ -> None

(** get_indexes_and_inputs: a' -> ('b * 'a) list -> (int * stack_type) list **)
let get_indexes_and_inputs a l =
  let rec get_index_ot a' b' l' i = try
    let element = List.nth l' i in
    match snd element = a' && fst element = b' with
      | true  -> (i,(snd element))::(get_index_ot a' b' l' (i+1))
      | false -> get_index_ot a' b' l' (i+1) with Failure _ -> [] in
  match get_random_element a l with
    | Some t -> get_index_ot a t l 1
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

let get_findex t_opt elems = 
  let rec indices il eindex =
    try 
      let elem = elems.(eindex) in
      let il' =
        match elem with 
          | None    -> il
          | Some e  ->
            (if (fst e) = t_opt
            then (eindex, snd e)::il
            else il)
      in
      indices il' (eindex + 1)
    with Invalid_argument _ -> il
  in
  indices [] 0

let get_ftype con funci =
  let rec find_findex flist t funci =
    match flist with 
      | []      -> None
      | e::rst  -> 
        if fst e = funci
        then Some (snd e, t)
        else find_findex rst t funci
  in
  let ftype = find_findex con.funcs.f_none None funci
  in
  if ftype <> None
  then ftype
  else (
    let ftype = find_findex con.funcs.f_i32 (Some I32Type) funci
    in
    if ftype <> None
    then ftype 
    else (
      let ftype = find_findex con.funcs.f_i64 (Some I64Type) funci
      in
      if ftype <> None
      then ftype 
      else (
        let ftype = find_findex con.funcs.f_f32 (Some F32Type) funci
        in
        if ftype <> None
        then ftype 
        else (
          let ftype = find_findex con.funcs.f_f64 (Some F64Type) funci
          in
          if ftype <> None
          then ftype 
          else None
        )
      )  
    ) 
  ) 
