open Wasm
open QCheck
open Helper

let addLabel t_option con =
  let con' = {
    labels = t_option::con.labels;
    locals = con.locals;
    globals = con.globals;
    funcs = con.funcs;
    imports = con.imports;
    mems = con.mems;
    data = con.data;
    return = con.return;
    tables = con.tables;
    elems = con.elems;
    funcindex = con.funcindex;
  } in
  con'

(*
type instr = instr' Source.phrase
and instr' =

  | Load of loadop                    (* read memory at address *)
  | Store of storeop                  (* write memory at address *)

  | CallIndirect of var               (* call function through table *)


  | Unreachable                       (* trap unconditionally *)
  | Return                            (* break from function body *)
  | Nop                               (* do nothing *)
  | Drop                              (* forget a value *)
  | Select                            (* branchless conditional *)
  | Const of literal                  (* constant *)
  | Test of testop                    (* numeric test *)
  | Compare of relop                  (* numeric comparison *)
  | Unary of unop                     (* unary numeric operator *)
  | Binary of binop                   (* binary numeric operator *)
  | Convert of cvtop                  (* conversion *)
  | Block of stack_type * instr list  (* execute in sequence *)
  | Loop of stack_type * instr list   (* loop header *)
  | If of stack_type * instr list * instr list  (* conditional *)
  | GetLocal of var                   (* read local variable *)
  | SetLocal of var                   (* write local variable *)
  | TeeLocal of var                   (* write local variable and keep value *)
  | Br of var                         (* break to n-th surrounding label *)
  | BrIf of var                       (* conditional break *)
  | BrTable of var list * var         (* indexed break *)
  | GetGlobal of var                  (* read global variable *)
  | SetGlobal of var                  (* write global variable *)
  | Call of var                       (* call function *)
  | MemorySize                        (* size of linear memory *)
  | MemoryGrow                        (* grow linear memory *)
*)

(*** Instructions list generator ***)
(** instrs_rule : context_ -> value_type list -> value_type list -> int -> (instr list) option Gen.t **)
let rec instrs_rule context input_ts output_ts size = 
    let recgen con t_opt tr = Gen.(instr_rule con t_opt (size/2) >>= function
          | None              -> return None
          | Some (con', instr', ts')  -> 
            instrs_rule con' input_ts (ts'@tr) (size/2) >>= (function
              | None        -> return None
              | Some instrs -> return (Some (instr'::instrs)) ) ) in
    match output_ts with
          []          -> 
            let empty_gen = recgen context None [] in
              Gen.(oneof [ empty_gen; return (Some []) ])
              (*if input_ts = output_ts 
              then Gen.(oneof [ empty_gen; return (Some []) ])
              else empty_gen*)
          | t1::trst  ->
            let empty_gen = recgen context None output_ts
            and non_empty_gen = recgen context (Some t1) trst in
              Gen.(frequency [ 1, empty_gen; 4, non_empty_gen; ])
              (*if input_ts = output_ts 
              then Gen.(frequency [ 1, empty_gen; 1, non_empty_gen; 1, return (Some []) ])
              else Gen.(frequency [ 1, empty_gen; 4, non_empty_gen; ])*)

(** instr_rule : context_ -> value_type option -> int -> (instr * value_type list) option Gen.t **)
and instr_rule con t_opt size = 
  let rules = match t_opt with
    | None   -> (match size with
      | 0 -> [(1, nop_gen con t_opt size); (10, setLocal_gen con t_opt size); (1, setGlobal_gen con t_opt size); (*(1, call_gen con t_opt size);*)]
      | n -> [(1, nop_gen con t_opt size); (1, drop_gen con t_opt size); (1, block_gen con t_opt size); 
              (1, loop_gen con t_opt size); (1, setLocal_gen con t_opt size); (1, setGlobal_gen con t_opt size); 
              (1, call_gen con t_opt size); (11, store_gen con t_opt size);(*(11, store_gen con t_opt size);*)])
    | Some _ -> (match size with 
      | 0 -> [(1, const_gen con t_opt size); (10, getLocal_gen con t_opt size); (1, getGlobal_gen con t_opt size);]
      | n -> [(1, const_gen con t_opt size); (9, unop_gen con t_opt size); (9, binop_gen con t_opt size); 
              (9, testop_gen con t_opt size); (9, relop_gen con t_opt size); (9, cvtop_gen con t_opt size); 
              (1, nop_gen con t_opt size); (5, block_gen con t_opt size); (5, loop_gen con t_opt size);
              (11, if_gen con t_opt size); (5, select_gen con t_opt size); (*(11, getLocal_gen con t_opt size);*)
              (*(5, setLocal_gen con t_opt size);*) (5, teeLocal_gen con t_opt size); (*(11, getGlobal_gen con t_opt size);*)
              (1, unreachable_gen con t_opt size); (1, return_gen con t_opt size); (11, br_gen con t_opt size); 
              (11, brif_gen con t_opt size); (11, brtable_gen con t_opt size); 
              (1, call_gen con t_opt size); (*(11, callindirect_gen con t_opt size);*)
              (1, memorysize_gen con t_opt size); (1, memorygrow_gen con t_opt size);])
  in generate_rule rules

and generate_rule rules =
  let rec remove ctw gw xs = match xs with
      | e::rst -> let tw = ctw + (fst e) in
        (match tw >= gw with
          | true  -> rst
          | false -> e::(remove tw gw rst))
      | []  -> [] in
  let rec verify ctw gw xs = match xs with
      | e::rst -> let tw = ctw + (fst e) in
        (match tw >= gw with
          | true  -> Gen.(snd e >>= fun t -> match t with
            | Some _ -> return t
            | None   -> generate_rule (remove 0 gw rules))    
          | false -> verify tw gw rst)
      | []  -> Gen.return None in
  let tw = List.fold_left (fun t (w, g) -> t + w ) 0 rules in
    match tw with
     | 0 -> Gen.return None
     | _ -> Gen.( int_range 1 tw >>= fun gw -> verify 0 gw rules)

(**** Numeric Instructions ****)

(*** Const ***)
(** const_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and const_gen con t_opt size = match t_opt with
  | Some Helper.I32Type -> Gen.( oneofl [ "0x7fffffff"; "0x80000000"; "0x80000001"; "0x3fffffff"; "0x01234567"; "0x8ff00ff0"; 
        "0x40000000"; "0xabcd9876"; "0xfe00dc00"; "0xb0c1d2e3"; "0x769abcdf"]
    >>= fun s -> frequency [
    11, return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I32 (Int32.of_string s)))), []));
    2, (small_int  >>= fun i -> return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I32 (Int32.of_int i)))), []))) ])
  | Some Helper.I64Type -> Gen.( oneofl [ (*"0x7fffffffffffffff";*) "0x8000000000000000"; "0x3fffffff"; "0x0123456789abcdef"; "0x8ff00ff00ff00ff0";
        "0xf0f0ffff"; "0x4000000000000000"; "0xabcd1234ef567809"; "0xabd1234ef567809c"; "0xabcd987602468ace"; "0xfe000000dc000000"; "0x00008000"; 
        "0x00010000"; "0xAAAAAAAA55555555"; "0x99999999AAAAAAAA"; "0xDEADBEEFDEADBEEF"]
    >>= fun s -> frequency [ 
    16, return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I64 (Int64.of_string s)))), []));
    2, (int >>= fun i -> return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I64 (Int64.of_int i)))), []))) ])
  | Some Helper.F32Type -> Gen.( oneofl [ "inf"; "-inf"; "0x0p+0"; "-0x0p+0"; "0x1p-149"; "-0x1p-149"; "0x1p-126"; "-0x1p-126"; 
        "0x1.921fb6p+2"; "-0x1.921fb6p+2"; "0x1.fffffep+127"; "-0x1.fffffep+127"]
    >>= fun s -> frequency [
    12, return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F32 (F32.of_string s)))), []));
    2, (float >>= fun i -> return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F32 (F32.of_float i)))), []))) ])
  | Some Helper.F64Type -> Gen.( oneofl [ "inf"; "-inf"; "0x0p+0"; "-0x0p+0"; "0x1p-1022"; "-0x1p-1022"; "0x1p-1"; "-0x1p-1"; 
        "0x0.0000000000001p-1022"; "-0x0.0000000000001p-1022"; "0x1.921fb54442d18p+2"; "-0x1.921fb54442d18p+2"; "0x1.fffffffffffffp+1023"; "-0x1.fffffffffffffp+1023"]
    >>= fun s -> frequency [
    14, return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F64 (F64.of_string s)))), []));
    2, (float      >>= fun i -> return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F64 (F64.of_float i)))), [])))   ])
  | Some Helper.IndexType -> (match con.mems with
    | Some mem -> let min = (Int32.to_int mem.min) in
      if min = 0 
      then Gen.return None
      else Gen.( int_bound ((min * 65536) - 8) >>= fun i ->
        return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I32 (Int32.of_int i)))), [])))
    | None     -> Gen.return None)
  | None               -> Gen.return None

(*** Unary operations ***)
(** intOp_unop_gen : IntOp.unop Gen.t **)
and intOp_unop_gen = Gen.oneofl
  [ Ast.IntOp.Clz; Ast.IntOp.Ctz; Ast.IntOp.Popcnt; ]

(** floatOp_unop_gen : FloatOp.unop Gen.t *)
and floatOp_unop_gen = Gen.oneofl
  [ Ast.FloatOp.Neg; Ast.FloatOp.Abs; Ast.FloatOp.Ceil; Ast.FloatOp.Floor; Ast.FloatOp.Trunc; Ast.FloatOp.Nearest; (*Ast.FloatOp.Sqrt;*) ]

(** unop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and unop_gen con t_opt size = match t_opt with
  | Some Helper.I32Type -> Gen.(intOp_unop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Unary (Values.I32 op)), [Helper.I32Type;])))
  | Some Helper.I64Type -> Gen.(intOp_unop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Unary (Values.I64 op)), [Helper.I64Type;])))
  | Some Helper.F32Type -> Gen.(floatOp_unop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Unary (Values.F32 op)), [Helper.F32Type;])))
  | Some Helper.F64Type -> Gen.(floatOp_unop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Unary (Values.F64 op)), [Helper.F64Type;])))
  | Some _ | None -> Gen.return None

(*** Binary operations ***)
(** intOp_binop_gen : IntOp.binop Gen.t **)
and intOp_binop_gen = Gen.oneofl
  [ Ast.IntOp.Add; Ast.IntOp.Sub; Ast.IntOp.Mul; Ast.IntOp.DivS; Ast.IntOp.DivU; Ast.IntOp.RemS; Ast.IntOp.RemU; 
    Ast.IntOp.And; Ast.IntOp.Or; Ast.IntOp.Xor; Ast.IntOp.Shl; Ast.IntOp.ShrS; Ast.IntOp.ShrU; Ast.IntOp.Rotl; Ast.IntOp.Rotr; ]

(** floatOp_binop_gen : FloatOp.binop Gen.t **)
and floatOp_binop_gen = Gen.oneofl
  [ Ast.FloatOp.Add; Ast.FloatOp.Sub; Ast.FloatOp.Mul; Ast.FloatOp.Div; Ast.FloatOp.Min; Ast.FloatOp.Max; Ast.FloatOp.CopySign; ]

(** binop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and binop_gen con t_opt size = match t_opt with
  | Some Helper.I32Type -> Gen.(intOp_binop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Binary (Values.I32 op)), [Helper.I32Type; Helper.I32Type])))
  | Some Helper.I64Type -> Gen.(intOp_binop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Binary (Values.I64 op)), [Helper.I64Type; Helper.I64Type])))
  | Some Helper.F32Type -> Gen.(floatOp_binop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Binary (Values.F32 op)), [Helper.F32Type; Helper.F32Type])))
  | Some Helper.F64Type -> Gen.(floatOp_binop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Binary (Values.F64 op)), [Helper.F64Type; Helper.F64Type])))
  | Some _ | None -> Gen.return None

(*** Test operations ***)
(** testop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and testop_gen con t_opt size = match t_opt with
  | Some Helper.I32Type -> Gen.( oneof [ return (Some (con, Helper.as_phrase (Ast.Test (Values.I32 Ast.IntOp.Eqz)), [Helper.I32Type;]));
                                        return (Some (con, Helper.as_phrase (Ast.Test (Values.I64 Ast.IntOp.Eqz)), [Helper.I64Type;])); ] )
  | Some _ | None -> Gen.return None

(*** Compare operations ***)
(** intOp_relop_gen : IntOp.relop Gen.t **)
and intOp_relop_gen = Gen.oneofl
  [ Ast.IntOp.Eq; Ast.IntOp.Ne; Ast.IntOp.LtS; Ast.IntOp.LtU; Ast.IntOp.GtS; Ast.IntOp.GtU; Ast.IntOp.LeS; Ast.IntOp.LeU; Ast.IntOp.GeS; Ast.IntOp.GeU; ]

(** floatOp_relop_gen : FloatOp.relop Gen.t **)
and floatOp_relop_gen = Gen.oneofl
  [ Ast.FloatOp.Eq; Ast.FloatOp.Ne; Ast.FloatOp.Lt; Ast.FloatOp.Gt; Ast.FloatOp.Le; Ast.FloatOp.Ge; ]

(** relop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and relop_gen con t_opt size = match t_opt with
  | Some Helper.I32Type -> Gen.(oneof [ 
    (intOp_relop_gen >>= fun op -> oneofl [ (Some (con, Helper.as_phrase (Ast.Compare (Values.I32 op)), [Helper.I32Type; Helper.I32Type])); 
                                            (Some (con, Helper.as_phrase (Ast.Compare (Values.I64 op)), [Helper.I64Type; Helper.I64Type])) ]);
    (floatOp_relop_gen >>= fun op -> oneofl [ (Some (con, Helper.as_phrase (Ast.Compare (Values.F32 op)), [Helper.F32Type; Helper.F32Type]));
                                              (Some (con, Helper.as_phrase (Ast.Compare (Values.F64 op)), [Helper.F64Type; Helper.F64Type])) ]); ])
  | Some _ | None -> Gen.return None

(*** Convert operations ***)
(** int32Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and int32Op_cvtop_gen con = Gen.oneofl
  [ (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.WrapI64)),   [Helper.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncSF32)), [Helper.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncUF32)), [Helper.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncSF64)), [Helper.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncUF64)), [Helper.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.ReinterpretFloat)), [Helper.F32Type])); ]

(** int64Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and int64Op_cvtop_gen con = Gen.oneofl
  [ (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.ExtendSI32)), [Helper.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.ExtendUI32)), [Helper.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncSF32)),  [Helper.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncUF32)),  [Helper.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncSF64)),  [Helper.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncUF64)),  [Helper.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.ReinterpretFloat)), [Helper.F64Type])); ]

(** float32Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and float32Op_cvtop_gen con = Gen.oneofl
  [ (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertSI32)), [Helper.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertUI32)), [Helper.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertSI64)), [Helper.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertUI64)), [Helper.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.DemoteF64)), [Helper.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ReinterpretInt)), [Helper.I32Type])); ]

(** float64Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and float64Op_cvtop_gen con = Gen.oneofl
  [ (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertSI32)), [Helper.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertUI32)), [Helper.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertSI64)), [Helper.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertUI64)), [Helper.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.PromoteF32)), [Helper.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ReinterpretInt)), [Helper.I64Type])); ]

(** cvtop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and cvtop_gen con t_opt size = match t_opt with
  | Some Helper.I32Type -> int32Op_cvtop_gen con
  | Some Helper.I64Type -> int64Op_cvtop_gen con
  | Some Helper.F32Type -> float32Op_cvtop_gen con
  | Some Helper.F64Type -> float64Op_cvtop_gen con
  | Some _ | None -> Gen.return None


(**** Parametric Instructions ****)

(*** Drop ***)
(** drop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and drop_gen con t_opt size = match t_opt with
| Some _ -> Gen.return None
| None   -> Gen.(oneofl [Helper.I32Type; Helper.I64Type; Helper.F32Type; Helper.F64Type] >>= fun t -> return (Some (con, Helper.as_phrase (Ast.Drop), [t])))

(*** Select ***)
(** select_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and select_gen con t_opt size = match t_opt with
  | Some t -> Gen.(return (Some (con, Helper.as_phrase (Ast.Select), [Helper.I32Type; t; t])))
  | None   -> Gen.return None

(**** Variable Instructions ****)

(*** GetLocal ***)
(** getLocal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and getLocal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> (let locals = Helper.get_indexes t con.locals in
      match locals with
        | e::es -> Gen.( oneofl locals >>= fun i -> return (Some (con, Helper.as_phrase (Ast.GetLocal (Helper.as_phrase (Int32.of_int i))), [])) )
        | []    -> Gen.return None )
  | None -> Gen.return None

(*** SetLocal ***)
(** setLocal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and setLocal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> Gen.return None
  | None -> match List.length con.locals with
    | 0 -> Gen.return None
    | n -> Gen.( int_range 1 n >>= fun g -> 
      let i = g - 1 in
      let t = List.nth con.locals i in
        return (Some (con, Helper.as_phrase (Ast.SetLocal (Helper.as_phrase (Int32.of_int i))), [t])) )

(*** TeeLocal ***)
(** teeLocal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and teeLocal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> (let locals = Helper.get_indexes t con.locals in
      match locals with
        | e::es -> Gen.( oneofl locals >>= fun i -> return (Some (con, Helper.as_phrase (Ast.TeeLocal (Helper.as_phrase (Int32.of_int i))), [t])) )
        | []    -> Gen.return None )
  | None -> Gen.return None

(*** GetGlobal ***)
(** getGlobal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and getGlobal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> (let globals = Helper.get_global_indexes t con.globals None in
    match globals with
      | e::es -> Gen.( oneofl globals >>= fun i -> return (Some (con, Helper.as_phrase (Ast.GetGlobal (Helper.as_phrase (Int32.of_int i))), [])) )
      | []    -> Gen.return None )
  | None -> Gen.return None

(*** SetGlobal ***)
(** setGlobal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and setGlobal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> Gen.return None
  | None -> Gen.( oneofl [Helper.I32Type; Helper.I64Type; Helper.F32Type; Helper.F64Type] >>= fun t -> 
              (let globals = Helper.get_global_indexes t con.globals (Some Types.Mutable) in
                match globals with
                  | e::es -> Gen.( oneofl globals >>= fun i -> return (Some (con, Helper.as_phrase (Ast.SetGlobal (Helper.as_phrase (Int32.of_int i))), [t])) )
                  | []    -> Gen.return None ))

(**** Memory Instructions ****)
(** align_load_gen **)
and align_load_gen t p_opt = 
  let width = match p_opt with
    | Some (p,_) -> Memory.packed_size p
    | None       -> Types.size t
  in
  Gen.(int_range 1 width >>= fun i -> return i)

(*** Load ***)
(** load_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and load_gen con t_opt size = match con.mems with
  | None   -> Gen.return None
  | Some m -> (match t_opt with
    | Some t -> Gen.(oneofl [Helper.I32Type; Helper.I64Type; Helper.F32Type; Helper.F64Type] >>= fun t ->
      frequency [ 
        1, return None; 
        3, pair (oneofl [Memory.Pack8; Memory.Pack16; Memory.Pack32]) (oneofl [ Memory.SX; Memory.ZX ]) >>= fun (p, sx) -> return (Some (p, sx)) 
      ] >>= fun p_opt ->
        pair (align_load_gen (Helper.to_wasm_value_type t) p_opt) (ui32)  >>= fun (align, offset) ->
          let mop = {
            Ast.ty = Helper.to_wasm_value_type t; 
            Ast.align = align; 
            Ast.offset = offset; 
            Ast.sz = p_opt
          } in
          return (Some (con, Helper.as_phrase (Ast.Load mop), [t; Helper.I32Type;])))
    | None   -> Gen.return None)

(** align_store_gen **)
and align_store_gen t p_opt = 
  let width = match p_opt with
    | Some p -> Memory.packed_size p 
    | None   -> Types.size t
  in
  Gen.(int_range 0 width >>= fun i -> return 0)

(*** Store ***)
(** store_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and store_gen con t_opt size = match con.mems with
  | None   -> Gen.return None
  | Some m -> (match t_opt with
    | Some _ -> Gen.return None
    | None   -> 
      if (Int32.to_int m.min) < 1
      then Gen.return None
      else Gen.(oneofl [Helper.I32Type; Helper.I64Type; (*Helper.F32Type; Helper.F64Type;*)] >>= fun t ->
        frequency [ 1, return None; 3, (oneofl [Memory.Pack8; Memory.Pack16; (*Memory.Pack32;*)] >>= fun p -> return (Some p)) ] >>= fun p_opt ->
          pair (align_store_gen (Helper.to_wasm_value_type t) p_opt) (ui32) >>= fun (align, offset) ->
            let mop = {
              Ast.ty = Helper.to_wasm_value_type t; 
              Ast.align = 0; 
              Ast.offset = 0l; 
              Ast.sz = p_opt
            } in
            return (Some (con, Helper.as_phrase (Ast.Store mop), [t; Helper.IndexType;]))))
    

(*** MemorySize ***)
(** memorysize_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and memorysize_gen con t_opt size = match t_opt with
  | Some Helper.I32Type  -> (match con.mems with
    | None -> Gen.return None
    | Some _  -> Gen.return (Some (con, Helper.as_phrase (Ast.MemorySize), [])))
  | Some _ | None       -> Gen.return None

(*** MemoryGrow ***)
(** memorygrow_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and memorygrow_gen con t_opt size = match t_opt with
  | Some Helper.I32Type  -> (match con.mems with
    | None -> Gen.return None
    | Some _  -> Gen.return (Some (con, Helper.as_phrase (Ast.MemoryGrow), [Helper.I32Type])))
  | Some _ | None       -> Gen.return None

(**** Control Instructions ****)

(*** Nop ***)
(** nop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and nop_gen con t_opt size = match t_opt with
  | Some t -> Gen.return (Some (con, Helper.as_phrase (Ast.Nop), [t]))
  | None   -> Gen.return (Some (con, Helper.as_phrase (Ast.Nop), []))

(*** Unreachable ***)
(** unreachable_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and unreachable_gen con t_opt size = match t_opt with
  | Some t -> Gen.return (Some (con, Helper.as_phrase (Ast.Unreachable), [t]))
  | None   -> Gen.return (Some (con, Helper.as_phrase (Ast.Unreachable), []))

(*** Block ***)
(** block_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and block_gen (con: context_) t_opt size = 
  let ot =  match t_opt with
    | Some t -> [t]
    | None   -> []
  in
  Gen.(instrs_rule (addLabel (t_opt, t_opt) con) [] ot size >>= 
    fun instrs_opt -> match instrs_opt with
      | Some instrs -> return (Some (con, Helper.as_phrase (Ast.Block (Helper.to_stack_type ot, (List.rev instrs))), []))
      | None        -> return (Some (con, Helper.as_phrase (Ast.Block (Helper.to_stack_type ot, [])), [])) )

(*** Loop ***)
(** loop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and loop_gen (con: context_) t_opt size = 
  let ot =  match t_opt with
    | Some t -> [t]
    | None   -> []
  in
  Gen.(instrs_rule (addLabel (None, t_opt) con) [] ot size >>= 
    fun instrs_opt -> match instrs_opt with
      | Some instrs -> return (Some (con, Helper.as_phrase (Ast.Loop (Helper.to_stack_type ot, (List.rev instrs))), []))
      | None        -> return (Some (con, Helper.as_phrase (Ast.Loop (Helper.to_stack_type ot, [])), [])) )

(*** If ***)
(** if_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and if_gen (con: context_) t_opt size = 
  let ot =  match t_opt with
    | Some t -> [t]
    | None   -> []
  in
  Gen.( pair (instrs_rule (addLabel ((Some Helper.I32Type), t_opt) con) [] ot (size/2)) 
             (instrs_rule (addLabel ((Some Helper.I32Type), t_opt) con) [] ot (size/2)) >>= 
      fun (instrs_opt1, instrs_opt2) -> 
        let instrs1 = match instrs_opt1 with
          | Some instrs -> instrs
          | None        -> [] 
        and instrs2 = match instrs_opt2 with
          | Some instrs -> instrs
          | None        -> [] in
        return (Some (con, Helper.as_phrase (Ast.If (Helper.to_stack_type ot, (List.rev instrs1), (List.rev instrs2))), [Helper.I32Type])) )

(*** Br ***)
(** br_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and br_gen (con: context_) t_opt size = 
  match Helper.get_indexes_and_inputs t_opt con.labels with
    | e::es -> Gen.( oneofl (e::es) >>= fun (i,it_opt) -> 
      let it = match it_opt with
        | Some t -> [t]
        | None   -> []
      in
      return (Some (con, Helper.as_phrase (Ast.Br (Helper.as_phrase (Int32.of_int i))), it)) )
    | []    -> Gen.return None

(*** BrIf ***)
(** brif_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and brif_gen (con: context_) t_opt size = 
  let labels = Helper.get_indexes_and_inputs t_opt con.labels in
    match labels with
      | e::es -> Gen.( oneofl labels >>= fun (i,it_opt) -> 
        let it = match it_opt with
          | Some t -> [t]
          | None   -> []
        in
        return (Some (con, Helper.as_phrase (Ast.BrIf (Helper.as_phrase (Int32.of_int i))), Helper.I32Type::it)) )
      | []    -> Gen.return None

(*** BrTable ***)
(** brtable_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and brtable_gen (con: context_) t_opt size = 
  let labels = Helper.get_indexes_and_inputs t_opt con.labels in
    match labels with
      | e::es -> Gen.( oneofl labels >>= fun (i,it_opt) -> 
        let it = match it_opt with
          | Some t -> [t]
          | None   -> []
        in    
        list (oneofl labels >>= fun (i',it_opt') -> return (Helper.as_phrase (Int32.of_int i'))) >>= fun ilist ->
          return (Some (con, Helper.as_phrase (Ast.BrTable (ilist, (Helper.as_phrase (Int32.of_int i)))), Helper.I32Type::it)) )
      | []    -> Gen.return None

(*** Return ***) 
(** return_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and return_gen (con: context_) t_opt size = 
  let tlist = try match snd (List.nth (con.imports@con.funcs) con.funcindex) with
    | Some t -> [t]
    | None   -> []
  with Failure _ -> []
  in
  Gen.return (Some (con, Helper.as_phrase Ast.Return, tlist))

(*** Call ***)
(** call_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and call_gen (con: context_) t_opt size = 
  match Helper.get_random_global t_opt (con.imports@con.funcs) con.funcindex with
   | Some (i,t) -> Gen.return (Some (con, Helper.as_phrase (Ast.Call (Helper.as_phrase (Int32.of_int i))), (List.rev t)))
   | None       -> Gen.return None

(*
(*** CallIndirect ***)
(** callindirect_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and callindirect_gen (con: context_) t_opt size = 
  match Helper.get_indexes_and_inputs2 t_opt con.tables with
    | e::es -> Gen.( oneofl (e::es) >>= fun (i,tlist) -> return (Some (con, Helper.as_phrase (Ast.CallIndirect (Helper.as_phrase (Int32.of_int i))), tlist)) )
    | []    -> Gen.return None
*)




(*(*** Statistics ***)
let length_stat list_opt = match list_opt with
  | None            -> 0
  | Some instr_list -> List.length instr_list

let rec none_stat list_opt = match list_opt with
  | None   -> 1
  | Some _ -> 0

let rec nop_stat list_opt = match list_opt with
  | None            -> 0
  | Some instr_list -> match instr_list with
      | []      -> 0
      | (e : Ast.instr)::es   -> let instr' = e.it in
          match instr' with
          | Ast.Nop       -> 1 + (nop_stat (Some (es)))
          | _             -> 0 + (nop_stat (Some (es)))

let rec drop_stat list_opt = match list_opt with
  | None            -> 0
  | Some instr_list -> match instr_list with
      | []      -> 0
      | (e : Ast.instr)::es   -> let instr' = e.it in
          match instr' with
          | Ast.Drop      -> 1 + (nop_stat (Some (es)))
          | _             -> 0 + (nop_stat (Some (es)))*)

let instr_gen context types = 
  Gen.(sized (fun n -> 
    instrs_rule context (fst types) (snd types) n >>= fun instrs -> return instrs))
