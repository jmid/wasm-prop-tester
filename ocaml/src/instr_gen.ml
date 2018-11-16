

open Wasm
open QCheck
open Helper

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
  | GetLocal of var                   (* read local variable *)
  | SetLocal of var                   (* write local variable *)
  | TeeLocal of var                   (* write local variable and keep value *)
  | GetGlobal of var                  (* read global variable *)
  | SetGlobal of var                  (* write global variable *)
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

(** nop_gen : module_ -> value_type option -> int -> (instr * value_type list) option Gen.t *)
let nop_gen con t_opt size = match t_opt with
  | None   -> Gen.return (Some (Helper.as_phrase (Ast.Nop), []))
  | Some t -> Gen.return (Some (Helper.as_phrase (Ast.Nop), [t]))

(** const_gen : module_ -> value_type option -> int -> (instr * value_type list) option Gen.t *)
and const_gen con t_opt size = match t_opt with
  | Some Types.I32Type -> Gen.map (fun i -> (Some (Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I32 (Int32.of_int i)))), []))) Gen.small_nat
  | Some Types.I64Type -> Gen.map (fun i -> (Some (Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I64 (Int64.of_int i)))), []))) Gen.nat
  | Some Types.F32Type -> Gen.map (fun i -> (Some (Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F32 (F32.of_float i)))), []))) Gen.float
  | Some Types.F64Type -> Gen.map (fun i -> (Some (Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.F64 (F64.of_float i)))), []))) Gen.float

(** intOp_unop_gen : IntOp.binop Gen.t *)
let intOp_unop_gen = Gen.oneofl
  [ Ast.IntOp.Clz; Ast.IntOp.Ctz; Ast.IntOp.Popcnt; ]

(** floatOp_unop_gen : FloatOp.unop Gen.t *)
let floatOp_unop_gen = Gen.oneofl
  [ Ast.FloatOp.Neg; Ast.FloatOp.Abs; Ast.FloatOp.Ceil; Ast.FloatOp.Floor; Ast.FloatOp.Trunc; Ast.FloatOp.Nearest; Ast.FloatOp.Sqrt; ]

(** int_unop_gen : module_ -> value_type option -> int -> (instr * value_type list) option Gen.t *)
let int_unop_gen con t_opt size = match t_opt with
  | Some Types.I32Type -> Gen.map (fun op -> (Some (Helper.as_phrase (Ast.Unary (Values.I32 op)), [Types.I32Type;]))) intOp_unop_gen
  | Some Types.I64Type -> Gen.map (fun op -> (Some (Helper.as_phrase (Ast.Unary (Values.I64 op)), [Types.I64Type;]))) intOp_unop_gen

(** float_unop_gen : module_ -> value_type option -> int -> (instr * value_type list) option Gen.t *)
let float_unop_gen con t_opt size = match t_opt with
  | Some Types.F32Type -> Gen.map (fun op -> (Some (Helper.as_phrase (Ast.Unary (Values.F32 op)), [Types.F32Type;]))) floatOp_unop_gen
  | Some Types.F64Type -> Gen.map (fun op -> (Some (Helper.as_phrase (Ast.Unary (Values.F64 op)), [Types.F64Type;]))) floatOp_unop_gen

(** unop_gen : module_ -> value_type option -> int -> (instr * value_type list) option Gen.t *)
let unop_gen con t_opt size = match t_opt with
  | Some Types.I32Type | Some Types.I64Type -> int_unop_gen con t_opt size
  | Some Types.F32Type | Some Types.F64Type -> float_unop_gen con t_opt size

(** intOp_binop_gen : IntOp.unop Gen.t *)
let intOp_binop_gen = Gen.oneofl
  [ Ast.IntOp.Add; Ast.IntOp.Sub; Ast.IntOp.Mul; Ast.IntOp.DivS; Ast.IntOp.DivU; Ast.IntOp.RemS; Ast.IntOp.RemU; 
    Ast.IntOp.And; Ast.IntOp.Or; Ast.IntOp.Xor; Ast.IntOp.Shl; Ast.IntOp.ShrS; Ast.IntOp.ShrU; Ast.IntOp.Rotl; Ast.IntOp.Rotr; ]

(** floatOp_binop_gen : FloatOp.binop Gen.t *)
let floatOp_binop_gen = Gen.oneofl
	[ Ast.FloatOp.Add; Ast.FloatOp.Sub; Ast.FloatOp.Mul; Ast.FloatOp.Div; Ast.FloatOp.Min; Ast.FloatOp.Max; Ast.FloatOp.CopySign; ]

(** int_binop_gen : module_ -> value_type option -> int -> (instr * value_type list) option Gen.t *)
let int_binop_gen con t_opt size = match t_opt with
  | Some Types.I32Type -> Gen.map (fun op -> (Some (Helper.as_phrase (Ast.Binary (Values.I32 op)), [Types.I32Type; Types.I32Type]))) intOp_binop_gen
  | Some Types.I64Type -> Gen.map (fun op -> (Some (Helper.as_phrase (Ast.Binary (Values.I64 op)), [Types.I64Type; Types.I64Type]))) intOp_binop_gen

(** float_binop_gen : module_ -> value_type option -> int -> (instr * value_type list) option Gen.t *)
let float_binop_gen con t_opt size = match t_opt with
  | Some Types.F32Type -> Gen.map (fun op -> (Some (Helper.as_phrase (Ast.Binary (Values.F32 op)), [Types.F32Type; Types.F32Type]))) floatOp_binop_gen
  | Some Types.F64Type -> Gen.map (fun op -> (Some (Helper.as_phrase (Ast.Binary (Values.F64 op)), [Types.F64Type; Types.F64Type]))) floatOp_binop_gen

(** binop_gen : module_ -> value_type option -> int -> (instr * value_type list) option Gen.t *)
let binop_gen con t_opt size = match t_opt with
  | Some Types.I32Type | Some Types.I64Type -> int_binop_gen con t_opt size
  | Some Types.F32Type | Some Types.F64Type -> float_binop_gen con t_opt size

(** instrs_rule : module_ -> value_type list -> value_type list -> int -> (instr list) option Gen.t *)
let rec instrs_rule con input_ts output_ts size = 
    let recgen t_opt tr = Gen.(instr_rule con t_opt (size/2) >>= function
          | None              -> return None
          | Some (instr, ts)  -> 
            instrs_rule con input_ts (ts@tr) (size/2) >>= (function
              | None        -> return None
              | Some instrs ->
                return (Some (instrs@[instr])) ) ) in
    match output_ts with
      []          -> 
        let empty_gen = recgen None [] in
          if input_ts = output_ts 
          then Gen.(oneof [ empty_gen; return (Some []) ])
          else empty_gen
      | t1::trst  ->
        let empty_gen = recgen None output_ts
        and non_empty_gen = recgen (Some t1) trst in
          if input_ts = output_ts 
          then Gen.(oneof [ empty_gen; non_empty_gen; return (Some []) ])
          else Gen.(oneof [ empty_gen; non_empty_gen; ])

(** instr_rule : module_ -> value_type option -> int -> (instr * value_type list) option Gen.t *)
and instr_rule con t_opt size = match t_opt with
    | None   -> let rules = [(1, nop_gen con t_opt size)] in
                  listPermuteTermGenInner con t_opt size rules
      (*Gen.(oneof [ nop_gen con t_opt size ] )*)
    | Some _ -> match size with 
      | 0 -> let rules = [(1, const_gen con t_opt size)]; in
                  listPermuteTermGenInner con t_opt size rules
      | n -> let rules = [ (1, const_gen con t_opt size); (9, unop_gen con t_opt size); (9, binop_gen con t_opt size); (1, nop_gen con t_opt size) ] in
                  listPermuteTermGenInner con t_opt size rules
      (*Gen.(oneof [ const_gen con t_opt size; binop_gen con t_opt size; nop_gen con t_opt size ] ) *)

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

let length_stat list_opt = match list_opt with
  | None            -> 0
  | Some instr_list -> List.length instr_list

let rec none_stat list_opt = match list_opt with
  | None   -> 1
  | Some _ -> 0

let instr_gen = Gen.sized (fun n -> instrs_rule [] [] [Types.I32Type] n)

let arb_intsr = make  ~print:instr_list_to_string ~stats:[("Length", length_stat); ("Nones", none_stat)] instr_gen

(*

let test =
  Test.make ~name:"Test" ~count:10
  (make (instrs_rule [] [Types.I32Type] ))
  (function 
      | Some instrs ->
        print_endline (instr_list_to_string instrs);
        true
      | None -> false
  )
;;
 
QCheck_runner.run_tests ~verbose:true [ test; ] ;;
*)
(* 
let test =
  Test.make ~name:"Test" ~count:10
  (make (instr_rule (Some Types.I32Type) ))
  (function 
      | Some (instr, t) ->
        print_endline (instr_list_to_string [instr]);
        print_endline (String.concat "," (List.map Types.string_of_value_type t));
        true
      | None -> false
  )
;;
*)