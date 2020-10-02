open Wasm
open QCheck
open Helper

let addLabel t_option con =
  { con with labels = t_option::con.labels; }

let i32_gen =
  let corner_gen =
    Gen.oneofl [ Int32.min_int; Int32.max_int;
               (*0x7fffffffl; 0x80000000l; 0x80000001l; (*0x00000000l;*)
                 0x3fffffffl; (*0x01234567l; 0x8ff00ff0l;*) 0x40000000l;*)
                 (*0xabcd9876l; 0xfe00dc00l; 0xb0c1d2e3l; 0x769abcdfl*) ] in
  Gen.(frequency [ 1, corner_gen;
                   3, map Int32.of_int small_signed_int;
                   5, ui32; ])

let i64_gen =
  let corner_gen =
    Gen.oneofl [ Int64.min_int; Int64.max_int;
                (*(*0x7fffffffffffffffL;*) 0x8000000000000000L; 0x3fffffffL;
                 0x0123456789abcdefL; 0x8ff00ff00ff00ff0L; 0xf0f0ffffL;
                 0x4000000000000000L; 0xabcd1234ef567809L; 0xabd1234ef567809cL;
                 0xabcd987602468aceL; 0xfe000000dc000000L; 0x00008000L;
                 0x00010000L; 0xAAAAAAAA55555555L; 0x99999999AAAAAAAAL;
                   0xDEADBEEFDEADBEEFL*) ] in
  Gen.(frequency [ 1, corner_gen;
                   3, map Int64.of_int small_signed_int;
                   5, ui64; ])

let f32_gen =
  let corner_gen =
    Gen.oneofl
      (List.map F32.of_string
         [ "inf"; "-inf"; "0x0p+0"; "-0x0p+0"; "0x1p-149"; "-0x1p-149"; "nan";
           (*"0x1p-126"; "-0x1p-126"; "0x1.921fb6p+2"; "-0x1.921fb6p+2";*)
           "0x1.fffffep+127"; "-0x1.fffffep+127"]) in
  Gen.(frequency [ 1, corner_gen;
                   3, map F32.of_float float;
                   5, map F32.of_bits ui32; ])

let f64_gen =
  let corner_gen =
    Gen.oneofl
      (List.map F64.of_string
         [ "inf"; "-inf"; "0x0p+0"; "-0x0p+0"; "0x1p-1022"; "-0x1p-1022"; "nan";
           "0x1p-1"; "-0x1p-1"; "0x0.0000000000001p-1022"; "-0x0.0000000000001p-1022";
           (*"0x1.921fb54442d18p+2"; "-0x1.921fb54442d18p+2";*)
           "0x1.fffffffffffffp+1023"; "-0x1.fffffffffffffp+1023"]) in
  Gen.(frequency [ 1, corner_gen;
                   3, map F64.of_float float;
                   5, map F64.of_bits ui64; ])

(** value_type_gen : Helper.value_type Gen.t **)
let value_type_gen = Gen.oneofl [I32Type; I64Type; F32Type; F64Type]

(** value_type_opt_gen : value_type option Gen.t **)
let value_type_opt_gen =
  Gen.(frequency
         [ 1, return None;
           3, map (fun t -> Some t) value_type_gen
         ])

(** stack_type_gen : int -> value_type list **)
let stack_type_gen n = Gen.list_repeat n value_type_gen

(*
type instr = instr' Source.phrase
and instr' =
  | Unreachable                       (* trap unconditionally *)
  | Nop                               (* do nothing *)
  | Drop                              (* forget a value *)
  | Select                            (* branchless conditional *)
  | Block of stack_type * instr list  (* execute in sequence *)
  | Loop of stack_type * instr list   (* loop header *)
  | If of stack_type * instr list * instr list  (* conditional *)
  | Br of var                         (* break to n-th surrounding label *)
  | BrIf of var                       (* conditional break *)
  | BrTable of var list * var         (* indexed break *)
  | Return                            (* break from function body *)
  | Call of var                       (* call function *)
  | CallIndirect of var               (* call function through table *)
  | LocalGet of var                   (* read local variable *)
  | LocalSet of var                   (* write local variable *)
  | LocalTee of var                   (* write local variable and keep value *)
  | GlobalGet of var                  (* read global variable *)
  | GlobalSet of var                  (* write global variable *)
  | Load of loadop                    (* read memory at address *)
  | Store of storeop                  (* write memory at address *)
  | MemorySize                        (* size of linear memory *)
  | MemoryGrow                        (* grow linear memory *)
  | Const of literal                  (* constant *)
  | Test of testop                    (* numeric test *)
  | Compare of relop                  (* numeric comparison *)
  | Unary of unop                     (* unary numeric operator *)
  | Binary of binop                   (* binary numeric operator *)
  | Convert of cvtop                  (* conversion *)
*)

(*** Instructions list generator ***)
(** instrs_rule : context_ -> value_type list -> int -> (instr list) option Gen.t **)
let rec instrs_rule context output_ts size =
  let recgen con t_opt tr = Gen.(instr_rule con t_opt (size/4) >>= function
    | None              -> return None
    | Some (con', instr', ts')  ->
      instrs_rule con' (ts'@tr) (3*size/4) >>= (function
          | None        -> return None
          | Some instrs -> return (Some (instr'::instrs)))) in
  match output_ts with
  | [] ->
    let empty_gen = recgen context None [] in
    Gen.(oneof [ empty_gen; return (Some []) ])
  | t1::trst  ->
    let empty_gen = recgen context None output_ts in
    let non_empty_gen = recgen context (Some t1) trst in
    Gen.frequency [ 1, empty_gen; 4, non_empty_gen; ]

(** instr_rule : context_ -> value_type option -> int -> (context * instr * value_type list) option Gen.t **)
and instr_rule con t_opt size =
  let rules = match t_opt with
    | None   -> (match size with
        | 0 ->
          [(1, nop_gen con t_opt size);
           (1, localSet_gen con t_opt size);
           (1, globalSet_gen con t_opt size);]
        | _ ->
          [(6, unreachable_gen con t_opt size);
           (1, nop_gen con t_opt size);
           (2, drop_gen con t_opt size);
           (2, block_gen con t_opt size);
           (2, loop_gen con t_opt size);
           (2, call_gen con t_opt size);
           (1, localSet_gen con t_opt size);
           (1, globalSet_gen con t_opt size);
           (6, store_gen con t_opt size);])
    | Some t -> (match size with
        | 0 ->
          [(1, const_gen con t_opt size);
           (1, localGet_gen con t_opt size);
           (1, globalGet_gen con t_opt size);]
        | n -> (match t with
            | I32Type      ->
              [(6, unreachable_gen con t_opt size);
               (1, nop_gen con t_opt size);
               (1, drop_gen con t_opt size);
               (8, select_gen con t_opt size);
               (2, block_gen con t_opt size);
               (2, loop_gen con t_opt size);
               (8, if_gen con t_opt size);
               (10, br_gen con t_opt size);
               (10, brif_gen con t_opt size);
               (10, brtable_gen con t_opt size);
               (6, return_gen con t_opt size);
               (8, call_gen con t_opt size);
               (14, callindirect_gen con t_opt size);
               (1, localGet_gen con t_opt size);
               (*(1, localSet_gen con t_opt size);*)
               (15, localTee_gen con t_opt size);
               (1, globalGet_gen con t_opt size);
               (*(1, globalSet_gen con t_opt size);*)
               (*(1, load_gen con t_opt size);*)
               (*(1, store_gen con t_opt size);*)
               (12, load_gen con t_opt size);
               (20, memorysize_gen con t_opt size);
               (20, memorygrow_gen con t_opt size);
               (1, const_gen con t_opt size);
               (20, testop_gen con t_opt size);
               (20, compare_gen con t_opt size);
               (8, unop_gen con t_opt size);
               (8, binop_gen con t_opt size);
               (8, cvtop_gen con t_opt size);
              ]
            | I64Type | F32Type | F64Type ->
              [
                (6, unreachable_gen con t_opt size);
                (1, nop_gen con t_opt size);
                (1, drop_gen con t_opt size);
                (8, select_gen con t_opt size);
                (2, block_gen con t_opt size);
                (6, loop_gen con t_opt size);
                (8, if_gen con t_opt size);
                (10, br_gen con t_opt size);
                (10, brif_gen con t_opt size);
                (10, brtable_gen con t_opt size);
                (6, return_gen con t_opt size);
                (8, call_gen con t_opt size);
                (14, callindirect_gen con t_opt size);
                (1, localGet_gen con t_opt size);
                (*(1, localSet_gen con t_opt size);*)
                (15, localTee_gen con t_opt size);
                (1, globalGet_gen con t_opt size);
                (*(1, globalSet_gen con t_opt size);*)
                (*(1, load_gen con t_opt size);*)
                (*(1, store_gen con t_opt size);*)
                (12, load_gen con t_opt size);
                (20, memorysize_gen con t_opt size);
                (20, memorygrow_gen con t_opt size);
                (1, const_gen con t_opt size);
                (* test_gen *)
                (* compare_gen *)
                (8, unop_gen con t_opt size);
                (8, binop_gen con t_opt size);
                (8, cvtop_gen con t_opt size);
              ]
            | MemIndexType
            | TableIndex _ ->
              [(1, const_gen con t_opt size);]))
  in generate_rule rules

and generate_rule rules =
  let open Gen in
  let rec try_in_order gs = match gs with
    | [] -> return None
    | g::gs' ->
      g >>= fun t -> match t with
      | None -> try_in_order gs'
      | Some _ -> return t in
  weighted_shuffle rules >>= try_in_order


(**** Numeric Instructions ****)
(*** Const ***)
(** const_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and const_gen con t_opt size = match t_opt with
  | None -> Gen.return None
  | Some I32Type ->
    Gen.map (fun i -> Some (con, as_phrase (Ast.Const (as_phrase (Values.I32 i))), [])) i32_gen
  | Some I64Type ->
    Gen.map (fun i -> Some (con, as_phrase (Ast.Const (as_phrase (Values.I64 i))), [])) i64_gen
  | Some F32Type ->
    Gen.map (fun f -> Some (con, as_phrase (Ast.Const (as_phrase (Values.F32 f))), [])) f32_gen
  | Some F64Type ->
    Gen.map (fun f -> Some (con, as_phrase (Ast.Const (as_phrase (Values.F64 f))), [])) f64_gen
  | Some MemIndexType -> (match con.mems with
      | None     -> Gen.return None
      | Some mem -> 
        let min = Int32.to_int mem.min in
        if min = 0
        then Gen.return None
        else Gen.( 
            map
              (fun i -> Some (con, as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int i)))), []))
              (int_bound ((min * 65536) - 8))))
  | Some TableIndex i -> Gen.return (Some (con, as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int i)))), []))

(*** Unary operations ***)
(** intOp_unop_gen : IntOp.unop Gen.t **)
and intOp_unop_gen =
  Gen.oneofl [ Ast.IntOp.Clz; Ast.IntOp.Ctz; Ast.IntOp.Popcnt; ]

(** floatOp_unop_gen : FloatOp.unop Gen.t *)
and floatOp_unop_gen =
  Gen.oneofl [ Ast.FloatOp.Neg; Ast.FloatOp.Abs; Ast.FloatOp.Ceil; Ast.FloatOp.Floor;
               Ast.FloatOp.Trunc; Ast.FloatOp.Nearest; (*Ast.FloatOp.Sqrt;*) ]

(** unop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and unop_gen con t_opt size = match t_opt with
  | Some I32Type ->
    Gen.map
      (fun op -> Some (con, as_phrase (Ast.Unary (Values.I32 op)), [I32Type]))
      intOp_unop_gen
  | Some I64Type ->
    Gen.map
      (fun op -> Some (con, as_phrase (Ast.Unary (Values.I64 op)), [I64Type]))
      intOp_unop_gen
  | Some F32Type ->
    Gen.map
      (fun op -> Some (con, as_phrase (Ast.Unary (Values.F32 op)), [F32Type]))
      floatOp_unop_gen
  | Some F64Type ->
    Gen.map
      (fun op -> Some (con, as_phrase (Ast.Unary (Values.F64 op)), [F64Type]))
      floatOp_unop_gen
  | Some _ | None -> Gen.return None

(*** Binary operations ***)
(** intOp_binop_gen : IntOp.binop Gen.t **)
and intOp_binop_gen =
  Gen.oneofl
    [ Ast.IntOp.Add; Ast.IntOp.Sub; Ast.IntOp.Mul; Ast.IntOp.DivS; Ast.IntOp.DivU;
      Ast.IntOp.RemS; Ast.IntOp.RemU; Ast.IntOp.And; Ast.IntOp.Or; Ast.IntOp.Xor;
      Ast.IntOp.Shl; Ast.IntOp.ShrS; Ast.IntOp.ShrU; Ast.IntOp.Rotl; Ast.IntOp.Rotr; ]

(** floatOp_binop_gen : FloatOp.binop Gen.t **)
and floatOp_binop_gen =
  Gen.oneofl
    [ Ast.FloatOp.Add; Ast.FloatOp.Sub; Ast.FloatOp.Mul; Ast.FloatOp.Div;
      Ast.FloatOp.Min; Ast.FloatOp.Max; Ast.FloatOp.CopySign; ]

(** binop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and binop_gen con t_opt size = match t_opt with
  | Some I32Type ->
    Gen.map
      (fun op -> Some (con, as_phrase (Ast.Binary (Values.I32 op)), [I32Type; I32Type]))
      intOp_binop_gen
  | Some I64Type ->
    Gen.map
      (fun op -> Some (con, as_phrase (Ast.Binary (Values.I64 op)), [I64Type; I64Type]))
      intOp_binop_gen
  | Some F32Type ->
    Gen.map
      (fun op -> Some (con, as_phrase (Ast.Binary (Values.F32 op)), [F32Type; F32Type]))
      floatOp_binop_gen
  | Some F64Type ->
    Gen.map
      (fun op -> Some (con, as_phrase (Ast.Binary (Values.F64 op)), [F64Type; F64Type]))
      floatOp_binop_gen
  | Some _ | None -> Gen.return None

(*** Test operations ***)
(** testop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and testop_gen con t_opt size = match t_opt with
  | Some I32Type ->
    Gen.oneofl [ Some (con, as_phrase (Ast.Test (Values.I32 Ast.IntOp.Eqz)), [I32Type]);
                 Some (con, as_phrase (Ast.Test (Values.I64 Ast.IntOp.Eqz)), [I64Type]); ]
  | Some _ | None -> Gen.return None

(*** Compare operations ***)
(** intOp_relop_gen : IntOp.relop Gen.t **)
and intOp_relop_gen =
  Gen.oneofl
    [ Ast.IntOp.Eq; Ast.IntOp.Ne; Ast.IntOp.LtS; Ast.IntOp.LtU; Ast.IntOp.GtS;
      Ast.IntOp.GtU; Ast.IntOp.LeS; Ast.IntOp.LeU; Ast.IntOp.GeS; Ast.IntOp.GeU; ]

(** floatOp_relop_gen : FloatOp.relop Gen.t **)
and floatOp_relop_gen =
  Gen.oneofl
    [ Ast.FloatOp.Eq; Ast.FloatOp.Ne; Ast.FloatOp.Lt; Ast.FloatOp.Gt; Ast.FloatOp.Le;
      Ast.FloatOp.Ge; ]

(** compare_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and compare_gen con t_opt size = match t_opt with
  | Some I32Type ->
    Gen.(oneof [
        (intOp_relop_gen >>= fun op ->
         oneofl [ Some (con, as_phrase (Ast.Compare (Values.I32 op)), [I32Type; I32Type]);
                  Some (con, as_phrase (Ast.Compare (Values.I64 op)), [I64Type; I64Type])]);
        (floatOp_relop_gen >>= fun op ->
         oneofl [ Some (con, as_phrase (Ast.Compare (Values.F32 op)), [F32Type; F32Type]);
                  Some (con, as_phrase (Ast.Compare (Values.F64 op)), [F64Type; F64Type])]); ])
  | Some _ | None -> Gen.return None

(*** Convert operations ***)
(** int32Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and int32Op_cvtop_gen con = Gen.oneofl
  [ Some (con, as_phrase (Ast.Convert (Values.I32 Ast.IntOp.WrapI64)),   [I64Type]);
    Some (con, as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncSF32)), [F32Type]);
    Some (con, as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncUF32)), [F32Type]);
    Some (con, as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncSF64)), [F64Type]);
    Some (con, as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncUF64)), [F64Type]);
    Some (con, as_phrase (Ast.Convert (Values.I32 Ast.IntOp.ReinterpretFloat)), [F32Type]); ]

(** int64Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and int64Op_cvtop_gen con = Gen.oneofl
  [ Some (con, as_phrase (Ast.Convert (Values.I64 Ast.IntOp.ExtendSI32)), [I32Type]);
    Some (con, as_phrase (Ast.Convert (Values.I64 Ast.IntOp.ExtendUI32)), [I32Type]);
    Some (con, as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncSF32)),  [F32Type]);
    Some (con, as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncUF32)),  [F32Type]);
    Some (con, as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncSF64)),  [F64Type]);
    Some (con, as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncUF64)),  [F64Type]);
    Some (con, as_phrase (Ast.Convert (Values.I64 Ast.IntOp.ReinterpretFloat)), [F64Type]); ]

(** float32Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and float32Op_cvtop_gen con = Gen.oneofl
  [ Some (con, as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertSI32)), [I32Type]);
    Some (con, as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertUI32)), [I32Type]);
    Some (con, as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertSI64)), [I64Type]);
    Some (con, as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertUI64)), [I64Type]);
    Some (con, as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.DemoteF64)),   [F64Type]);
    Some (con, as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ReinterpretInt)), [I32Type]); ]

(** float64Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and float64Op_cvtop_gen con = Gen.oneofl
  [ Some (con, as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertSI32)), [I32Type]);
    Some (con, as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertUI32)), [I32Type]);
    Some (con, as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertSI64)), [I64Type]);
    Some (con, as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertUI64)), [I64Type]);
    Some (con, as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.PromoteF32)),  [F32Type]);
    Some (con, as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ReinterpretInt)), [I64Type]); ]

(** cvtop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and cvtop_gen con t_opt size = match t_opt with
  | Some I32Type -> int32Op_cvtop_gen con
  | Some I64Type -> int64Op_cvtop_gen con
  | Some F32Type -> float32Op_cvtop_gen con
  | Some F64Type -> float64Op_cvtop_gen con
  | Some _ | None -> Gen.return None


(**** Parametric Instructions ****)
(*** Drop ***)
(** drop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and drop_gen con t_opt size =
  let stack_rest = match t_opt with
    | Some t' -> [t']
    | None    -> [] in
  Gen.map (fun t -> Some (con, as_phrase Ast.Drop, t::stack_rest)) value_type_gen

(*** Select ***)
(** select_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and select_gen con t_opt size = match t_opt with
  | None   -> Gen.return None
  | Some t -> Gen.return (Some (con, as_phrase Ast.Select, [I32Type; t; t]))

(**** Variable Instructions ****)

(*** LocalGet ***)
(** localGet_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and localGet_gen (con: context_) t_opt size = match t_opt with
  | None -> Gen.return None
  | Some t ->
    let locals = get_indexes t con.locals in
    if locals = []
    then Gen.return None
    else
      Gen.(map
             (fun i -> Some (con, as_phrase (Ast.LocalGet (as_phrase (Int32.of_int i))), []))
             (oneofl locals))

(*** LocalSet ***)
(** localSet_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and localSet_gen (con: context_) t_opt size = match t_opt with
  | Some t -> Gen.return None
  | None -> match List.length con.locals with
    | 0 -> Gen.return None
    | n ->
      Gen.(int_range 1 n >>= fun g ->
           let i = g - 1 in
           let t = List.nth con.locals i in
           return (Some (con, as_phrase (Ast.LocalSet (as_phrase (Int32.of_int i))), [t])))

(*** LocalTee ***)
(** localTee_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and localTee_gen (con: context_) t_opt size = match t_opt with
  | None -> Gen.return None
  | Some t ->
    let locals = get_indexes t con.locals in
    if locals = []
    then Gen.return None
    else
      Gen.(map
             (fun i -> Some (con, as_phrase (Ast.LocalTee (as_phrase (Int32.of_int i))), [t]))
             (oneofl locals))

(*** GlobalGet ***)
(** globalGet_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and globalGet_gen (con: context_) t_opt size = match t_opt with
  | None -> Gen.return None
  | Some t ->
    let globals = get_global_indexes t con.globals None in
    if globals = []
    then Gen.return None
    else
      Gen.(map
             (fun i -> Some (con, as_phrase (Ast.GlobalGet (as_phrase (Int32.of_int i))), []))
             (oneofl globals))

(*** GlobalSet ***)
(** globalSet_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and globalSet_gen (con: context_) t_opt size = match t_opt with
  | Some t -> Gen.return None
  | None ->
    Gen.(value_type_gen >>= fun t ->
         let globals = get_global_indexes t con.globals (Some Types.Mutable) in
         if globals = []
         then Gen.return None
         else
           Gen.(map
                  (fun i -> Some (con, as_phrase (Ast.GlobalSet (as_phrase (Int32.of_int i))), [t]))
                  (oneofl globals)))

(**** Memory Instructions ****)
(** align_load_gen **)
and align_load_gen t p_opt =
  let width = match p_opt with
    | Some (p,_) -> Types.packed_size p
    | None       -> Types.size t in
  Gen.int_bound (log2 (width / 8))

(*** Load ***)
(** load_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and load_gen con t_opt size = match con.mems with
  | None   -> Gen.return None
  | Some m -> (match t_opt with
      | None   -> Gen.return None
      | Some t ->
        Gen.((match t with
            | I32Type -> opt (pair (oneofl Types.([Pack8;Pack16]))        (oneofl Types.([SX;ZX])))
            | I64Type -> opt (pair (oneofl Types.([Pack8;Pack16;Pack32])) (oneofl Types.([SX;ZX])))
            | F32Type
            | F64Type (* storage size only allowed for integer loads *)
            | MemIndexType
            | TableIndex _ -> return None) >>= fun p_opt ->
          pair (align_load_gen (to_wasm_value_type t) p_opt) ui32 >>= fun (align, offset) ->
          let mop = { Ast.ty     = to_wasm_value_type t;
                      Ast.align  = align;
                      Ast.offset = offset;
                      Ast.sz     = p_opt } in
          return (Some (con, as_phrase (Ast.Load mop), [(*MemIndexType*)I32Type]))))

(** align_store_gen **)
and align_store_gen t p_opt =
  let width = match p_opt with
    | Some p -> Types.packed_size p
    | None   -> Types.size t in
  Gen.int_bound (log2 (width / 8))

(*** Store ***)
(** store_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and store_gen con t_opt size = match con.mems with
  | None   -> Gen.return None
  | Some m -> (match t_opt with
    | Some _ -> Gen.return None
    | None   ->
      if (Int32.to_int m.min) < 1
      then Gen.return None
      else Gen.(value_type_gen >>= fun t -> (match t with
          | I32Type -> opt (oneofl Types.([Pack8;Pack16]))
          | I64Type -> opt (oneofl Types.([Pack8;Pack16;Pack32]))
          | F32Type
          | F64Type (* storage size only allowed for integer stores *)
          | MemIndexType
          | TableIndex _ -> return None) >>= fun p_opt ->
          pair (align_store_gen (to_wasm_value_type t) p_opt) ui32 >>= fun (align, offset) ->
            let mop = { Ast.ty     = to_wasm_value_type t;
                        Ast.align  = align;
                        Ast.offset = offset;
                        Ast.sz     = p_opt } in
            return (Some (con, as_phrase (Ast.Store mop), [t; MemIndexType;]))))


(*** MemorySize ***)
(** memorysize_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and memorysize_gen con t_opt size = match t_opt with
  | Some I32Type  -> (match con.mems with
    | None    -> Gen.return None
    | Some _  -> Gen.return (Some (con, as_phrase Ast.MemorySize, [])))
  | Some _ | None       -> Gen.return None

(*** MemoryGrow ***)
(** memorygrow_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and memorygrow_gen con t_opt size = match t_opt with
  | Some I32Type  -> (match con.mems with
    | None    -> Gen.return None
    | Some _  -> Gen.return (Some (con, as_phrase Ast.MemoryGrow, [I32Type])))
  | Some _ | None       -> Gen.return None

(**** Control Instructions ****)
(*** Nop ***)
(** nop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and nop_gen con t_opt size = match t_opt with
  | Some t -> Gen.return (Some (con, as_phrase Ast.Nop, [t]))
  | None   -> Gen.return (Some (con, as_phrase Ast.Nop, []))

(*** Unreachable ***)
(** unreachable_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and unreachable_gen con t_opt size = match t_opt with
  | Some t -> Gen.return (Some (con, as_phrase Ast.Unreachable, [t]))
  | None   -> Gen.return (Some (con, as_phrase Ast.Unreachable, []))
(*FIXME: this "input type" is less general than the specification *)

(*** Block ***)
(** block_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and block_gen (con: context_) t_opt size =
  let ot =  match t_opt with
    | Some t -> [t]
    | None   -> []
  in
  Gen.(instrs_rule (addLabel (t_opt, t_opt) con) ot (size/2) >>=
    function
      | Some instrs -> return (Some (con, as_phrase (Ast.Block (ValBlockType (to_opt_stack_type t_opt), List.rev instrs)), []))
      | None        -> return (Some (con, as_phrase (Ast.Block (ValBlockType (to_opt_stack_type t_opt), [])), [])))
   (*FIXME: on generation failure, an empty block does not leave [t] on the stack *)

(*** Loop ***)
(** loop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and loop_gen (con: context_) t_opt size =
  let ot =  match t_opt with
    | Some t -> [t]
    | None   -> []
  in
  Gen.(instrs_rule (addLabel (None, t_opt) con) ot (size/2) >>=
    function
      | Some instrs -> return (Some (con, as_phrase (Ast.Loop (ValBlockType (to_opt_stack_type t_opt), List.rev instrs)), []))
      | None        -> return (Some (con, as_phrase (Ast.Loop (ValBlockType (to_opt_stack_type t_opt), [])), [])))
   (*FIXME: on generation failure, an empty loop body does not leave [t] on the stack? *)

(*** If ***)
(** if_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and if_gen (con: context_) t_opt size =
  let ot =  match t_opt with
    | Some t -> [t]
    | None   -> []
  in (* FIXME: increase chance of empty else (or then) branch *)
  Gen.(pair (instrs_rule (addLabel (Some I32Type, t_opt) con) ot (size/2))
            (instrs_rule (addLabel (Some I32Type, t_opt) con) ot (size/2)) >>=
      fun (instrs_opt1, instrs_opt2) ->
        let instrs1 = match instrs_opt1 with
          | Some instrs -> instrs
          | None        -> [] in
        let instrs2 = match instrs_opt2 with
          | Some instrs -> instrs
          | None        -> [] in
        return (Some (con, as_phrase (Ast.If (ValBlockType (to_opt_stack_type t_opt), List.rev instrs1, List.rev instrs2)), [I32Type])))

(*** Br ***)
(** br_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and br_gen (con: context_) t_opt size =
  Gen.(get_indexes_and_inputs t_opt con.labels >>= fun labels ->
       if labels = []
       then return None
       else
         oneofl labels >>= fun (i,it_opt) ->
         let it = match it_opt with
           | Some t -> [t]
           | None   -> [] in
         return (Some (con, as_phrase (Ast.Br (as_phrase (Int32.of_int i))), it)))
(* FIXME: less polymorphic than spec *)

(*** BrIf ***)
(** brif_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and brif_gen (con: context_) t_opt size =
  Gen.(get_indexes_and_inputs t_opt con.labels >>= fun labels ->
       if labels = []
       then return None
       else
         oneofl labels >>= fun (i,it_opt) ->
         let it = match it_opt with
           | Some t -> [t]
           | None   -> [] in
         return (Some (con, as_phrase (Ast.BrIf (as_phrase (Int32.of_int i))), I32Type::it)))

(*** BrTable ***)
(** brtable_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and brtable_gen (con: context_) t_opt size =
  Gen.(get_indexes_and_inputs t_opt con.labels >>= fun labels ->
       if labels = []
       then return None
       else
         oneofl labels >>= fun (i,it_opt) ->
         let it = match it_opt with
           | Some t -> [t]
           | None   -> [] in
         list (oneofl labels >>= fun (i',it_opt') ->
               return (as_phrase (Int32.of_int i'))) >>= fun ilist ->
         return (Some (con, as_phrase (Ast.BrTable (ilist, (as_phrase (Int32.of_int i)))), I32Type::it)))
(* FIXME: less polymorphic than spec *)

(*** Return ***)
(** return_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and return_gen (con: context_) t_opt size =
  let tlist = match con.return with
    | Some t -> [t]
    | None   -> [] in
  Gen.return (Some (con, as_phrase Ast.Return, tlist))
(* FIXME: less polymorphic than spec *)

(*** Call ***)
(** call_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and call_gen (con: context_) t_opt size =
  let ilist = match t_opt with
    | None               -> con.funcs.f_none
    | Some I32Type       -> con.funcs.f_i32
    | Some I64Type       -> con.funcs.f_i64
    | Some F32Type       -> con.funcs.f_f32
    | Some F64Type       -> con.funcs.f_f64
    | Some MemIndexType  -> []
    | Some TableIndex _  -> [] in
  if ilist = []
  then Gen.return None
  else (* FIXME: discourage self call w/weighting, rather than eliminate chance *)
    let ilist' = List.filter (fun (i, tl) -> i <> con.funcindex) ilist in
    if ilist' = []
    then Gen.return None
    else
      Gen.(
        oneofl ilist' >>= fun (i, tl') ->
          return (Some (con, as_phrase (Ast.Call (as_phrase (Int32.of_int i))), List.rev tl')))

(*** CallIndirect ***)
(** callindirect_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and callindirect_gen (con: context_) t_opt size =
  match con.elems with
    | None   -> Gen.return None
    | Some a ->
      let ilist = get_findex t_opt a in
      if ilist = []
      then Gen.return None
      else Gen.(oneofl ilist >>= fun (idx,i) ->
          match get_ftype con i with
            | None       -> return None
            | Some ftype ->
              return (Some (con,
                            as_phrase (Ast.CallIndirect (as_phrase (Int32.of_int i))),
                            (TableIndex idx)::(List.rev (fst ftype)))))

let instr_gen context goal_types = Gen.sized (instrs_rule context goal_types)
  (* Gen.(sized_size (int_bound 500) (fun n ->
    instrs_rule context (snd types) n >>= fun instrs -> return instrs)) *)
  (* Gen.(sized_size (int_bound 1000) (fun n ->
    instrs_rule context (snd types) n >>= fun instrs -> return instrs)) *)
