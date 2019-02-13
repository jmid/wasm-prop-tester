open Wasm
open QCheck
open Instr_gen
open Helper

let tmp_dir_name = "tmp";;


let tmp_dir = match Sys.file_exists tmp_dir_name with
  | true  -> ()
  | false -> Unix.mkdir tmp_dir_name 0o775;;

let file_name = tmp_dir_name ^ "/" ^ "tmp_sexpr.wat";;



(*

#use "arithmetic_expressions.ml";;
#use "generator/instr_gen.ml";; 
let file_name = "tmp_sexpr.wat";;
*)

(* type 'a phrase = {at : region; it : 'a} *)
(*
type module_' = {
  types : type_ list;
  globals : global list;
  tables : table list;
  memories : memory list;
  funcs : func list;
  start : var option;
  elems : var list segment list;
  data : string segment list;
  imports : import list;
  exports : export list;
}

type func = func' Source.phrase
and func' =
{
  ftype : var;
  locals : value_type list;
  body : instr list;
}

let as_phrase x = {Source.at = Source.no_region; Source.it = x}
;;

let exp = Add ( Lit (1) , Lit (1));;

(*Arithmetic expression To Wasm function*)
let exp_to_func ae =
    let rec exp_to_f ae = match ae with
      | Lit i -> [as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int i))))]
      | Add (ae0, ae1) ->
        let s0 = exp_to_f ae0 in
        let s1 = exp_to_f ae1 in
        s0 @ s1 @ [ as_phrase (Ast.Binary (Values.I32 Ast.IntOp.Add))]
      | Sub (ae0, ae1) ->
        let s0 = exp_to_f ae0 in
        let s1 = exp_to_f ae1 in
        s0 @ s1 @ [ as_phrase (Ast.Binary (Values.I32 Ast.IntOp.Sub))]
      | Mul (ae0, ae1) ->
        let s0 = exp_to_f ae0 in
        let s1 = exp_to_f ae1 in
        s0 @ s1 @ [ as_phrase (Ast.Binary (Values.I32 Ast.IntOp.Mul))]
      | Div (ae0, ae1) ->
        let s0 = exp_to_f ae0 in
        let s1 = exp_to_f ae1 in
        s0 @ s1 @ [ as_phrase (Ast.Binary (Values.I32 Ast.IntOp.DivS))]
    in exp_to_f ae
;;

let expf = exp_to_func exp;;

let empty_f = 
  {
    Ast.ftype = Helper.as_phrase 0l;
    Ast.locals = [];
    (*Ast.body = [as_phrase (Ast.Const (as_phrase (Values.I32 42l)))];*)
    Ast.body = expf;
  }
;;

let empty = get_module types_ [Helper.as_phrase (get_func (exp_to_func exp))] in
  let empty_module = Helper.as_phrase (empty) in
    let arrange_m = Arrange.module_ empty_module in
      Sexpr.print 80 arrange_m
;;

let arithmetic_ast =
  Test.make ~name:"Arithmetic expressions" ~count:10 
  (set_shrink tshrink arb_tree)
  (fun e -> 
    let empty = get_module types_ [Helper.as_phrase (get_func (exp_to_func e))] in
      let empty_module = Helper.as_phrase (empty) in
        let arrange_m = Arrange.module_ empty_module in
          wasm_to_file arrange_m
    ;
    Sys.command ("../script/compare.sh " ^ file_name) = 0
  )
;;
*)

let string_to_name s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (Char.code(s.[i]) :: l) in
  exp (String.length s - 1) []
;;

let get_func input ftype body = 
  {
    Ast.ftype = ftype;
    (*Ast.locals = [];*)
    Ast.locals = input;
    Ast.body = body;
  }
;;

let wasm_to_file m = 
  let oc = open_out file_name in 
    Sexpr.output oc 80 m;
    close_out oc
;;

let context = {
  labels = [];
  locals = [Types.I32Type; Types.F32Type];
  globals = [];
  funcs = [];
  mems = [];
  return = None;
  tables = [];
  funcindex = 0;
}
;;

(** value_type_opt_gen : value_type option **)
let value_type_opt_gen = Gen.(
  frequency [ 
    1, return None; 
    3, (oneofl [Types.I32Type; Types.I64Type; Types.F32Type; Types.F64Type] >>= fun t -> return (Some t)) 
  ] >>= fun t_opt -> return t_opt)
;;

(** stack_type_gen : int -> value_type list **)
let stack_type_gen n = Gen.(list_repeat n (oneofl [Types.I32Type; Types.I64Type; Types.F32Type; Types.F64Type] >>= fun t -> return t))
;;

(** func_type_gen : func_type **)
let func_type_gen = Gen.(pair small_int (int_bound 1)>>= fun (n, m) -> 
  pair (stack_type_gen n) (stack_type_gen m) >>= fun t -> return t)
;;

(** func_type_gen : func_type **)
let func_type_gen2 = Gen.(pair small_int (int_bound 1)>>= fun (n, m) -> 
  pair (stack_type_gen n) (value_type_opt_gen) >>= fun t -> return t)
;;

(** func_type_list_gen : func_type list **)
let func_type_list_gen = Gen.(list_size small_nat func_type_gen2)
;;

(** func_type_list_gen : func_type list **)
let func_type_list_gen2 = Gen.(list_size (int_bound 2) func_type_gen2)
;;

let context_gen = 
  Gen.(func_type_list_gen >>= fun funcs ->
    return {
      labels = [Some (Types.I32Type), Some (Types.I32Type)];
      locals = [];
      globals = [];
      funcs = ([Types.I32Type; Types.F32Type], (Some Types.I32Type))::funcs;
      mems = [];
      return = None;
      tables = [];
      funcindex = 0;
    }
  )

let context2_gen = 
  Gen.(func_type_list_gen2 >>= fun funcs ->
    return {
      labels = [];
      locals = [];
      globals = [];
      funcs = ([], Some (Types.I32Type))::funcs;
      mems = [];
      return = None;
      tables = [];
      funcindex = 0;
    }
  )

let rec func_type_list_to_type_phrase func_type_list = match func_type_list with
  | e::rst -> let ot_opt = match (snd e) with
    | Some t -> [t]
    | None   -> [] in
      (as_phrase (Types.FuncType (fst e, ot_opt)))::(func_type_list_to_type_phrase rst)
  | []     -> []

let context_with_ftype con funcindex = 
  let ftype = List.nth con.funcs funcindex
  in
    let label = [snd ftype, snd ftype]
    in
      let con' = {
        labels = label;
        locals = con.locals;
        globals = con.globals;
        funcs = con.funcs;
        mems = con.mems;
        return = None;
        tables = con.tables;
        funcindex = funcindex;
      } in
      con'

(* 
let module_gen = Gen.(context2_gen >>= fun con -> 
  (* rec_func_gen : (Types.stack_type * Types.stack_type) list -> ((instr list) option) list Gen.t *)
  let rec rec_func_gen func_types index = match func_types with
    | e::rst -> let func_t = match snd e with
                  | Some t -> [t]
                  | None   -> []
                in
                (let instrs_opt = generate1 (Instr_gen.instr_gen (context_with_ftype con (snd e)) (fst e, func_t)) in
                  (let instrs = match instrs_opt with
                    | Some inst -> inst
                    | None      -> [] 
                  in
                  as_phrase (get_func (fst e) (as_phrase (Int32.of_int index)) instrs))
                )::(rec_func_gen rst (index + 1) ) 
    | []     -> [] in
    let funcs = rec_func_gen con.funcs 0 in
      return (as_phrase (get_module (func_type_list_to_type_phrase con.funcs) funcs))
  )
*)

let module_gen = Gen.(context2_gen >>= fun con -> 
  (* rec_func_gen : (Types.stack_type * Types.stack_type) list -> ((instr list) option) list Gen.t *)
  let rec rec_func_gen func_types index = match func_types with
    | e::rst -> let func_t = match snd e with
                  | Some t -> [t]
                  | None   -> []
                in
                (let instrs_opt = generate1 (Instr_gen.instr_gen (context_with_ftype con index) (fst e, func_t)) in
                  (let instrs = match instrs_opt with
                    | Some inst -> inst
                    | None      -> [] 
                  in
                  as_phrase (get_func (fst e) (as_phrase (Int32.of_int index)) instrs))
                )::(rec_func_gen rst (index + 1) ) 
    | []     -> [] in
    let funcs = rec_func_gen con.funcs 0 in
      return (as_phrase (get_module (func_type_list_to_type_phrase con.funcs) funcs))
  )

let arb_module = make module_gen

let module_test =
  Test.make ~name:"Modules" ~count:1000 
  arb_module
  (function m ->
    (*print_endline "start";*)
    let arrange_m = Arrange.module_ m in
      wasm_to_file arrange_m
    ;
    Sys.command ("../script/compare.sh " ^ file_name) = 0
  )
;;

QCheck_runner.run_tests ~verbose:true [ module_test; ] ;;

(*
~stats:[("Length", length_stat); ("Nones", none_stat); ("Nops", nop_stat); ("Drops", drop_stat)]

let arithmetic_spec_ast =
  Test.make ~name:"Arithmetic expressions" ~count:1
  arb_module
  (function
    | None    -> true
    | Some e  ->
      let empty = get_module types_ [as_phrase (get_func e)] in
        let empty_module = as_phrase (empty) in
          let arrange_m = Arrange.module_ empty_module in
            wasm_to_file arrange_m
      ;
      Sys.command ("../script/compare.sh " ^ file_name) = 0
  )
;;

QCheck_runner.set_seed(410086340);;
22165827
(*QCheck_runner.set_seed(401353417);;*)
QCheck_runner.run_tests ~verbose:true [ (*arithmetic_spec_ast;*) (*module_test;*)module_test; ] ;;
*)
(*

QCheck_runner.run_tests ~verbose:true [ arithmetic_ast; arithmetic_spec_ast; ] ;;
*)
(*

let f = F32.of_float (-1.0)
let s = F32.sqrt f

;;

print_endline (F32.to_string s)
 
*)