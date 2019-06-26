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
  let rec get_index l i = match l with
    | [] -> []
    | e::l' ->
      if e = a
      then i::(get_index l' (i+1))
      else get_index l' (i+1) in
  get_index l 0

let typeToString = Types.string_of_value_type

let mToString = function
  | None -> "None"
  | Some Types.Mutable   -> "Mutable"
  | Some Types.Immutable -> "Immutable"

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

(** get_random_element: 'a -> ('b * 'a) list -> 'b option Gen.t **)
let get_random_element a l =
  let rec get_index l = match l with
    | [] -> []
    | (fst,snd)::l' ->
      if snd = a
      then fst::(get_index l')
      else get_index l' in
  let type_list = get_index l in
  if type_list = []
  then Gen.return None
  else Gen.(map (fun t -> Some t) (oneofl type_list))

(** get_indexes_and_inputs: 'a -> ('b * 'a) list -> (int * stack_type) list Gen.t **)
let get_indexes_and_inputs a l =
  let rec get_index_ot b l' i = match l' with
    | [] -> []
    | (fst,snd)::es ->
      if snd = a && fst = b
      then (i,snd)::(get_index_ot b es (i+1))
      else           get_index_ot b es (i+1) in
  Gen.(get_random_element a l >>= function  (* Why does this req. generation? *)
    | Some t -> return (get_index_ot t l 0)
    | None   -> return [])

let get_findex t_opt elems =
  let rec indices il eindex =
    try 
      let elem = elems.(eindex) in
      let il' =
        match elem with 
          | None    -> il
          | Some (fst,snd) ->
            if fst = t_opt
            then (eindex, snd)::il
            else il
      in
      indices il' (eindex + 1)
    with Invalid_argument _ -> il
  in
  indices [] 0

let get_ftype con funci =
  let rec find_findex flist t funci = match flist with
    | [] -> None
    | (fst,snd)::rst ->
      if fst = funci
      then Some (snd, t)
      else find_findex rst t funci in
  let ftype = find_findex con.funcs.f_none None funci in
  if ftype <> None
  then ftype
  else
    let ftype = find_findex con.funcs.f_i32 (Some I32Type) funci in
    if ftype <> None
    then ftype 
    else
      let ftype = find_findex con.funcs.f_i64 (Some I64Type) funci in
      if ftype <> None
      then ftype 
      else
        let ftype = find_findex con.funcs.f_f32 (Some F32Type) funci in
        if ftype <> None
        then ftype 
        else
          (*let ftype =*)find_findex con.funcs.f_f64 (Some F64Type) funci(* in
          if ftype <> None
          then ftype
          else None*)
