open Wasm
open QCheck

type context_ = {
  labels: (Types.value_type option) list;
  locals: Types.stack_type;
  globals: Types.stack_type;
}

let addLabel t_option con =
  let con' = {
    labels = t_option::con.labels;
    locals = con.locals;
    globals = con.globals;
  } in
  con'

(*
type instr = instr' Source.phrase
and instr' =
  | Unreachable                       (* trap unconditionally *)
  | Br of var                         (* break to n-th surrounding label *)
  | BrIf of var                       (* conditional break *)
  | BrTable of var list * var         (* indexed break *)
  | Return                            (* break from function body *)
  | Call of var                       (* call function *)
  | CallIndirect of var               (* call function through table *)
  | Load of loadop                    (* read memory at address *)
  | Store of storeop                  (* write memory at address *)
  | MemorySize                        (* size of linear memory *)
  | MemoryGrow                        (* grow linear memory *)


  | GetLocal of var                   (* read local variable *)
  | SetLocal of var                   (* write local variable *)
  | TeeLocal of var                   (* write local variable and keep value *)


  | GetGlobal of var                  (* read global variable *)
  | SetGlobal of var                  (* write global variable *)
  

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
*)

(*** Instructions list generator ***)
(** instrs_rule : context_ -> value_type list -> value_type list -> int -> (instr list) option Gen.t **)
let rec instrs_rule context input_ts output_ts size = 
    let recgen con t_opt tr = Gen.(instr_rule con t_opt (size/2) >>= function
          | None              -> return None
          | Some (con', instr', ts')  -> 
            instrs_rule con' input_ts (ts'@tr) (size/2) >>= (function
              | None        -> return None
              | Some instrs ->
                return (Some (instrs@[instr'])) ) ) in
    match output_ts with
      []          -> 
        let empty_gen = recgen context None [] in
          if input_ts = output_ts 
          then Gen.(oneof [ empty_gen; return (Some []) ])
          else empty_gen
      | t1::trst  ->
        let empty_gen = recgen context None output_ts
        and non_empty_gen = recgen context (Some t1) trst in
          if input_ts = output_ts 
          then Gen.(frequency [ 1, empty_gen; 8, non_empty_gen; 1, return (Some []) ])
          else Gen.(frequency [ 1, empty_gen; 4, non_empty_gen; ])

(** instr_rule : context_ -> value_type option -> int -> (instr * value_type list) option Gen.t **)
and instr_rule con t_opt size = match t_opt with
    | None   -> (match size with
      | 0 -> let rules = [(1, nop_gen con t_opt size); (1, drop_gen con t_opt size)] in
                  listPermuteTermGenInner con t_opt size rules
      | n -> let rules = [(1, nop_gen con t_opt size); (1, drop_gen con t_opt size); (1, block_gen con t_opt size); 
                          (1, loop_gen con t_opt size)] in
                  listPermuteTermGenInner con t_opt size rules)
    | Some _ -> (match size with 
      | 0 -> let rules = [(1, const_gen con t_opt size)]; in
                  listPermuteTermGenInner con t_opt size rules
      | n -> let rules = [ (1, const_gen con t_opt size); (9, unop_gen con t_opt size); (9, binop_gen con t_opt size); 
                           (9, testop_gen con t_opt size); (9, relop_gen con t_opt size); (9, cvtop_gen con t_opt size); 
                           (1, nop_gen con t_opt size); (1, block_gen con t_opt size); (1, loop_gen con t_opt size);
                           (1, if_gen con t_opt size); (12, select_gen con t_opt size) ] in
                  listPermuteTermGenInner con t_opt size rules)

and listPermuteTermGenInner con goal size rules =
  let rec removeAt n xs = match (n, xs) with
    | (0, x::xs) -> xs
    | (n, x::xs) -> x :: removeAt (n - 1) xs
    | _          -> failwith "index out of bounds" in
  let elementsWeighted xs =
    let _,ig = List.fold_left
      (fun (i,acc) (w,g) -> (i+1,(w,Gen.pair (Gen.return i) g)::acc)) (0,[]) xs in
    Gen.frequency ig in
  let toTerm i = function
      | Some term -> Gen.return (Some term)
      | None      ->
        let remainingRules = removeAt i rules in
        listPermuteTermGenInner con goal size remainingRules in

  if rules = []
  then Gen.return None
  else Gen.(elementsWeighted rules >>= (fun (i,t) -> toTerm i t))

(**** Numeric Instructions ****)

(*** Const ***)
(** const_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and const_gen con t_opt size = match t_opt with
  | Some Types.I32Type -> Gen.( oneofl [ "0x7fffffff"; "0x80000000"; "0x80000001"; "0x3fffffff"; "0x01234567"; "0x8ff00ff0"; 
        "0x40000000"; "0xabcd9876"; "0xfe00dc00"; "0xb0c1d2e3"; "0x769abcdf"]
    >>= fun s -> frequency [
    11, return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I32 (Int32.of_string s)))), []));
    2, (small_int  >>= fun i -> return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I32 (Int32.of_int i)))), []))) ])
  | Some Types.I64Type -> Gen.( oneofl [ "0x7fffffffffffffff"; "0x8000000000000000"; "0x3fffffff"; "0x0123456789abcdef"; "0x8ff00ff00ff00ff0";
        "0xf0f0ffff"; "0x4000000000000000"; "0xabcd1234ef567809"; "0xabd1234ef567809c"; "0xabcd987602468ace"; "0xfe000000dc000000"; "0x00008000"; 
        "0x00010000"; "0xAAAAAAAA55555555"; "0x99999999AAAAAAAA"; "0xDEADBEEFDEADBEEF"]
    >>= fun s -> frequency [ 
    16, return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I64 (Int64.of_string s)))), []));
    2, (int >>= fun i -> return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I64 (Int64.of_int i)))), []))) ])
  | Some Types.F32Type -> Gen.( oneofl [ "inf"; "-inf"; "0x0p+0"; "-0x0p+0"; "0x1p-149"; "-0x1p-149"; "0x1p-126"; "-0x1p-126"; 
        "0x1.921fb6p+2"; "-0x1.921fb6p+2"; "0x1.fffffep+127"; "-0x1.fffffep+127"]
    >>= fun s -> frequency [
    12, return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F32 (F32.of_string s)))), []));
    2, (float >>= fun i -> return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F32 (F32.of_float i)))), []))) ])
  | Some Types.F64Type -> Gen.( oneofl [ "inf"; "-inf"; "0x0p+0"; "-0x0p+0"; "0x1p-1022"; "-0x1p-1022"; "0x1p-1"; "-0x1p-1"; 
        "0x0.0000000000001p-1022"; "-0x0.0000000000001p-1022"; "0x1.921fb54442d18p+2"; "-0x1.921fb54442d18p+2"; "0x1.fffffffffffffp+1023"; "-0x1.fffffffffffffp+1023"]
    >>= fun s -> frequency [
    14, return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F64 (F64.of_string s)))), []));
    2, (float      >>= fun i -> return (Some (con, Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F64 (F64.of_float i)))), [])))   ])
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
  | Some Types.I32Type -> Gen.(intOp_unop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Unary (Values.I32 op)), [Types.I32Type;])))
  | Some Types.I64Type -> Gen.(intOp_unop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Unary (Values.I64 op)), [Types.I64Type;])))
  | Some Types.F32Type -> Gen.(floatOp_unop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Unary (Values.F32 op)), [Types.F32Type;])))
  | Some Types.F64Type -> Gen.(floatOp_unop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Unary (Values.F64 op)), [Types.F64Type;])))
  | None -> Gen.return None

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
  | Some Types.I32Type -> Gen.(intOp_binop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Binary (Values.I32 op)), [Types.I32Type; Types.I32Type])))
  | Some Types.I64Type -> Gen.(intOp_binop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Binary (Values.I64 op)), [Types.I64Type; Types.I64Type])))
  | Some Types.F32Type -> Gen.(floatOp_binop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Binary (Values.F32 op)), [Types.F32Type; Types.F32Type])))
  | Some Types.F64Type -> Gen.(floatOp_binop_gen >>= fun op -> return (Some (con, Helper.as_phrase (Ast.Binary (Values.F64 op)), [Types.F64Type; Types.F64Type])))
  | None -> Gen.return None

(*** Test operations ***)
(** testop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and testop_gen con t_opt size = match t_opt with
  | Some Types.I32Type -> Gen.( oneof [ return (Some (con, Helper.as_phrase (Ast.Test (Values.I32 Ast.IntOp.Eqz)), [Types.I32Type;]));
                                        return (Some (con, Helper.as_phrase (Ast.Test (Values.I64 Ast.IntOp.Eqz)), [Types.I64Type;])); ] )
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
  | Some Types.I32Type -> Gen.(oneof [ 
    (intOp_relop_gen >>= fun op -> oneofl [ (Some (con, Helper.as_phrase (Ast.Compare (Values.I32 op)), [Types.I32Type; Types.I32Type])); 
                                            (Some (con, Helper.as_phrase (Ast.Compare (Values.I64 op)), [Types.I64Type; Types.I64Type])) ]);
    (floatOp_relop_gen >>= fun op -> oneofl [ (Some (con, Helper.as_phrase (Ast.Compare (Values.F32 op)), [Types.F32Type; Types.F32Type]));
                                              (Some (con, Helper.as_phrase (Ast.Compare (Values.F64 op)), [Types.F64Type; Types.F64Type])) ]); ])
  | Some _ | None -> Gen.return None

(*** Convert operations ***)
(** int32Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and int32Op_cvtop_gen con = Gen.oneofl
  [ (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.WrapI64)), [Types.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncSF32)), [Types.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncUF32)), [Types.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncSF64)), [Types.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.TruncUF64)), [Types.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I32 Ast.IntOp.ReinterpretFloat)), [Types.F32Type])); ]

(** int64Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and int64Op_cvtop_gen con = Gen.oneofl
  [ (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.ExtendSI32)), [Types.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.ExtendUI32)), [Types.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncSF32)), [Types.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncUF32)), [Types.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncSF64)), [Types.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.TruncUF64)), [Types.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.I64 Ast.IntOp.ReinterpretFloat)), [Types.F64Type])); ]

(** float32Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and float32Op_cvtop_gen con = Gen.oneofl
  [ (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertSI32)), [Types.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertUI32)), [Types.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertSI64)), [Types.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ConvertUI64)), [Types.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.DemoteF64)), [Types.F64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F32 Ast.FloatOp.ReinterpretInt)), [Types.I32Type])); ]

(** float64Op_cvtop_gen : context_ -> (context_ * instr * value_type list) option Gen.t **)
and float64Op_cvtop_gen con = Gen.oneofl
  [ (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertSI32)), [Types.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertUI32)), [Types.I32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertSI64)), [Types.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ConvertUI64)), [Types.I64Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.PromoteF32)), [Types.F32Type]));
    (Some (con, Helper.as_phrase (Ast.Convert (Values.F64 Ast.FloatOp.ReinterpretInt)), [Types.I64Type])); ]

(** cvtop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and cvtop_gen con t_opt size = match t_opt with
  | Some Types.I32Type -> int32Op_cvtop_gen con
  | Some Types.I64Type -> int64Op_cvtop_gen con
  | Some Types.F32Type -> float32Op_cvtop_gen con
  | Some Types.F64Type -> float64Op_cvtop_gen con
  | None -> Gen.return None


(**** Parametric Instructions ****)

(*** Drop ***)
(** drop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and drop_gen con t_opt size = match t_opt with
| Some _ -> Gen.return None
| None   -> Gen.(oneofl [Types.I32Type; Types.I64Type; Types.F32Type; Types.F64Type] >>= fun t -> return (Some (con, Helper.as_phrase (Ast.Drop), [t])))

(*** Select ***)
(** select_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and select_gen con t_opt size = match t_opt with
  | Some t -> Gen.(return (Some (con, Helper.as_phrase (Ast.Select), [Types.I32Type; t; t])))
  | None   -> Gen.return None

(**** Variable Instructions ****)

(*** GetLocal ***)
(** getLocal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and getLocal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> (match Helper.get_indexes t con.locals with
    | Some l  -> Gen.( oneofl l >>= fun i -> return (Some (con, Helper.as_phrase (Ast.GetLocal (Helper.as_phrase (Int32.of_int i))), [])) )
    | None    -> Gen.return None )
  | None -> Gen.return None

(*** SetLocal ***)
(** setLocal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and setLocal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> Gen.return None
  | None -> Gen.( oneofl con.locals >>= 
    fun t -> (match Helper.get_indexes t con.locals with
                | Some l  -> Gen.( oneofl l >>= fun i -> return (Some (con, Helper.as_phrase (Ast.SetLocal (Helper.as_phrase (Int32.of_int i))), [t])) )
                | None    -> Gen.return None ))

(*** TeeLocal ***)
(** teeLocal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and teeLocal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> (match Helper.get_indexes t con.locals with
    | Some l  -> Gen.( oneofl l >>= fun i -> return (Some (con, Helper.as_phrase (Ast.GetLocal (Helper.as_phrase (Int32.of_int i))), [t])) )
    | None    -> Gen.return None )
  | None -> Gen.return None

(*** GetGlobal ***)
(** getGlobal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and getGlobal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> (match Helper.get_indexes t con.globals with
    | Some l  -> Gen.( oneofl l >>= fun i -> return (Some (con, Helper.as_phrase (Ast.GetGlobal (Helper.as_phrase (Int32.of_int i))), [])) )
    | None    -> Gen.return None )
  | None -> Gen.return None

(*** SetGlobal ***)
(** setGlobal_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and setGlobal_gen (con: context_) t_opt size = match t_opt with
  | Some t -> Gen.return None
  | None -> Gen.( oneofl con.globals >>= 
    fun t -> (match Helper.get_indexes t con.globals with
                | Some l  -> Gen.( oneofl l >>= fun i -> return (Some (con, Helper.as_phrase (Ast.SetGlobal (Helper.as_phrase (Int32.of_int i))), [t])) )
                | None    -> Gen.return None ))

(**** Memory Instructions ****)

(** TODO*)

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
  let block_instr t = Gen.(
    instrs_rule con [] t size >>= 
      fun instrs_opt -> match instrs_opt with
        | Some instrs -> return (Some ((addLabel t_opt con), Helper.as_phrase (Ast.Block (t, instrs)), []))
        | None        -> return (Some ((addLabel t_opt con), Helper.as_phrase (Ast.Block (t, [])), [])) ) in
  match t_opt with
  | Some t -> block_instr [t]
  | None   -> block_instr []

(*** Loop ***)
(** loop_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and loop_gen (con: context_) t_opt size = 
  let loop_instr t = Gen.(
    instrs_rule con [] t size >>= 
      fun instrs_opt -> match instrs_opt with
        | Some instrs -> return (Some ((addLabel t_opt con), Helper.as_phrase (Ast.Loop (t, instrs)), []))
        | None        -> return (Some ((addLabel t_opt con), Helper.as_phrase (Ast.Loop (t, [])), [])) ) in
  match t_opt with
  | Some t -> loop_instr [t]
  | None   -> loop_instr []

(*** If ***)
(** if_gen : context_ -> value_type option -> int -> (context_ * instr * value_type list) option Gen.t **)
and if_gen (con: context_) t_opt size = 
  let if_gen t = Gen.(
    pair (instrs_rule con [] t (size/2)) (instrs_rule con [] t (size/2)) >>= 
      fun (instrs_opt1, instrs_opt2) -> 
        let instrs1 = match instrs_opt1 with
          | Some instrs -> instrs
          | None        -> [] 
        and instrs2 = match instrs_opt2 with
          | Some instrs -> instrs
          | None        -> [] in
        return (Some ((addLabel t_opt con), Helper.as_phrase (Ast.If (t, instrs1, instrs2)), [Types.I32Type])) ) in
  match t_opt with
    | Some t -> if_gen [t]
    | None   -> if_gen []














(*** Statistics ***)
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
          | _             -> 0 + (nop_stat (Some (es)))

let context = {
  labels = [];
  locals = [Types.I32Type; Types.F32Type];
  globals = [];
}

let instr_gen = Gen.sized (fun n -> instrs_rule context [] [Types.I32Type] n)

let arb_intsr = make ~stats:[("Length", length_stat); ("Nones", none_stat); ("Nops", nop_stat); ("Drops", drop_stat)] instr_gen



(* 
let instr_to_string (instr : Ast.instr) = 
  let instr' = instr.it in
    match instr' with
    | Ast.Const v   -> Values.string_of_value v.it ^ " "
    | Ast.Binary b  -> (match b with
      | Values.I32 Ast.IntOp.Add  -> "Add "
      | Values.I32 Ast.IntOp.Sub  -> "Sub "
      | Values.I32 Ast.IntOp.Mul  -> "Mul "
      | Values.I32 Ast.IntOp.DivS -> "DivS "
      | Values.I32 Ast.IntOp.DivU -> "DivU "
      | Values.I32 Ast.IntOp.RemS -> "RemS "
      | Values.I32 Ast.IntOp.RemU -> "RemU "
      | Values.I32 Ast.IntOp.And  -> "And "
      | Values.I32 Ast.IntOp.Or   -> "Or "
      | Values.I32 Ast.IntOp.Xor  -> "Xor "
      | Values.I32 Ast.IntOp.Shl  -> "Shl "
      | Values.I32 Ast.IntOp.ShrS -> "ShrS "
      | Values.I32 Ast.IntOp.ShrU -> "ShrU "
      | Values.I32 Ast.IntOp.Rotl -> "Rotl "
      | Values.I32 Ast.IntOp.Rotr -> "Rotr ")
    | Ast.Nop       -> "Nop "
    | _             -> ""
;;

let rec instr_list_to_string list_opt = match list_opt with
  | None            -> ""
  | Some instr_list -> match instr_list with
    | e::es -> (instr_to_string e) ^ (instr_list_to_string (Some es))
    | []    -> ""
;;

*)