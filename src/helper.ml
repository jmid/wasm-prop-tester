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

let to_opt_stack_type = Option.map to_wasm_value_type
let to_list_stack_type = List.map to_wasm_value_type

let string_to_name s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (Char.code(s.[i]) :: l) in
  exp (String.length s - 1) []

let as_phrase x = { Source.at = Source.no_region; Source.it = x }

let types_ = [ 
  as_phrase (Types.FuncType ([], []));
  as_phrase (Types.FuncType ([], [Types.I32Type]));
  as_phrase (Types.FuncType ([], [Types.I64Type]));
  as_phrase (Types.FuncType ([], [Types.F32Type]));
  as_phrase (Types.FuncType ([], [Types.F64Type]))
]


(** get_indexes: 'a -> 'a list -> int list **)
let get_indexes a l =
  snd (List.fold_left (fun (i,acc) e -> (i+1, if e = a then i::acc else acc)) (0,[]) l)
  
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
  let type_list =
    List.fold_left (fun acc (fst,snd) -> if snd = a then fst::acc else acc) [] l in
  if type_list = []
  then Gen.return None
  else Gen.(map (fun t -> Some t) (oneofl type_list))

(** get_indexes_and_inputs: 'a -> ('b * 'a) list -> (int * stack_type) list Gen.t **)
let get_indexes_and_inputs a l =
  let get_index_ot b =
    snd (List.fold_left
           (fun (i,acc) (fst,snd) ->
              (i+1, if snd = a && fst = b then (i,snd)::acc else acc)) (0,[]) l) in
  Gen.map (function  
            | None   -> []  (* Why does this req. generation? *)
            | Some t -> get_index_ot t) (get_random_element a l)

(** get_findex : 'a -> ('a * 'b) option array -> (int * 'b) list **)
let get_findex t_opt elems =
  snd (Array.fold_left (fun (i,il) elem ->
                         (i+1, match elem with
                               | None -> il
                               | Some (fst,snd) ->
                                 if fst = t_opt then (i, snd)::il else il)) (0,[]) elems)

let get_ftype con funci =
  let find_findex flist t =
    match List.find_opt (fun (f,_) -> f = funci) flist with
    | None -> None
    | Some (_f,snd) -> Some (snd,t) in
  let ftype = find_findex con.funcs.f_none None in
  if ftype <> None
  then ftype
  else
    let ftype = find_findex con.funcs.f_i32 (Some I32Type) in
    if ftype <> None
    then ftype 
    else
      let ftype = find_findex con.funcs.f_i64 (Some I64Type) in
      if ftype <> None
      then ftype 
      else
        let ftype = find_findex con.funcs.f_f32 (Some F32Type) in
        if ftype <> None
        then ftype 
        else find_findex con.funcs.f_f64 (Some F64Type)

let rec instrs_length il = match il with
  | []      -> 0
  | (e : Ast.instr)::rst  -> match e.it with
    | Ast.Block (t, l)   -> (instrs_length rst) + (instrs_length l) + 1
    | Ast.Loop (t, l)    -> (instrs_length rst) + (instrs_length l) + 1
    | Ast.If (t, l1, l2) -> (instrs_length rst) + (instrs_length l1) + (instrs_length l2) + 1
    | _                  -> (instrs_length rst) + 1


let gen_float rs = Random.State.float rs 1.0

(* Implements a weighted random shuffle according to
   http://utopia.duth.gr/~pefraimi/research/data/2007EncOfAlg.pdf
   https://softwareengineering.stackexchange.com/questions/233541/how-to-implement-a-weighted-shuffle/344274#344274
 *)
let weighted_shuffle gs =
  let open Gen in
  let rec walk gs = match gs with
    | [] -> return []
    | (w,g)::gs' ->
      gen_float >>= fun u ->
      let w' = -. (u ** (1. /. float_of_int w)) in
      walk gs' >>= fun gs'' -> return ((w',g)::gs'') in
  walk gs >>= fun gs' ->
  return (List.map snd (List.sort (fun (f,_) (f',_) -> compare f f') gs'))

(* For positive powers of 2  :-\ *)
let rec log2 n = match n with
  | 0 -> 0
  | 1 -> 0
  | _ -> 1 + log2 (n lsr 1)

(*  find_index : ('a -> bool) -> 'a list -> int  *)
let find_index p gs =
  let rec loop i gs = match gs with
    | [] -> failwith "find_index: not found"
    | g::gs -> if p g then i else loop (i+1) gs in
  loop 0 gs

let rec take n xs = match n,xs with
  | 0, _     -> []
  | _, []    -> []
  | _, x::xs -> x::take (n-1) xs

let first_type_index l ls =
  let my_type = List.nth ls (Int32.to_int l.Source.it) in
  let i = find_index (fun t -> t = my_type) ls in
  my_type,Int32.of_int i
