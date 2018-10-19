#load "wasm.cmo";;
#use "util/helper.ml";;

open Wasm
open QCheck

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

(* Constants generator *)
let const_gen = Gen.map (fun i -> [Helper.as_phrase (Ast.Const (Helper.as_phrase (Values.I32 (Int32.of_int i))))]) Gen.small_nat

let a = Helper.as_phrase (Ast.Binary (Values.I32 Ast.IntOp.Add));;

(* Binary operator generator *)
let binop_gen =
  Gen.oneofl
	[
    [Helper.as_phrase (Ast.Binary (Values.I32 Ast.IntOp.Add))];
    [Helper.as_phrase (Ast.Binary (Values.I32 Ast.IntOp.Sub))];
    [Helper.as_phrase (Ast.Binary (Values.I32 Ast.IntOp.Mul))];
  ]
;;

let instr_to_string (instr : Ast.instr) = 
  let instr' = instr.it in
    match instr' with
    | Ast.Const v   -> Values.string_of_value v.it ^ " "
    | Ast.Binary b  -> (match b with
      | Values.I32 Ast.IntOp.Add -> "Add "
      | Values.I32 Ast.IntOp.Sub -> "Sub "
      | Values.I32 Ast.IntOp.Mul -> "Mul ")
    | _             -> ""
;;

let rec instr_list_to_string instr_list = match instr_list with
  | e::es -> (instr_to_string e) ^ (instr_list_to_string es)
  | []    -> ""

let op_gen =
  Gen.map3 (fun l r op -> l @ r @ op ) const_gen const_gen binop_gen

let instr_gen =
  Gen.sized (Gen.fix 
    (fun recgen n -> match n with
      | 0 -> const_gen
      | n -> Gen.map3 (fun l r op -> l @ r @ op ) (recgen(n/2)) (recgen(n/2)) binop_gen
    ))
;;

let arb_intsr = make ~print:instr_list_to_string instr_gen

(*
let test =
  Test.make ~name:"Test" ~count:10
  arb
  (fun e -> 
    print_endline (instr_list_to_string e);
    true
  )
;;
 
QCheck_runner.run_tests ~verbose:true [ test; ] ;;
*)