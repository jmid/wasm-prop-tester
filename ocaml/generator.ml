#load "wasm.cmo";;
#use "arithmetic_expressions.ml";;

open Wasm

let tmp_dir_name = "tmp";;


let tmp_dir = match Sys.file_exists tmp_dir_name with
  | true  -> ()
  | false -> Unix.mkdir tmp_dir_name 0o775;;

let file_name = tmp_dir_name ^ "/" ^ "tmp_sexpr.wat";;



(* 
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
*)

let string_to_name s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (Char.code(s.[i]) :: l) in
  exp (String.length s - 1) []
;;

let as_phrase x = {Source.at = Source.no_region; Source.it = x}
;;

let get_module types funcs = {
  Ast.types = types;
  Ast.globals = [];
  Ast.tables = [];
  Ast.memories = [];
  Ast.funcs = funcs;
  Ast.start = None;
  Ast.elems  = [];
  Ast.data = [];
  Ast.imports = [];
  Ast.exports = [
    as_phrase ({
      Ast.name = string_to_name "aexp";
      Ast.edesc = as_phrase (Ast.FuncExport (as_phrase 0l));
    })
  ];
}
;;

let types_ = [ 
  as_phrase (Types.FuncType ([], [Types.I32Type]));
  as_phrase (Types.FuncType ([], [Types.I64Type]));
  as_phrase (Types.FuncType ([], [Types.F32Type]));
  as_phrase (Types.FuncType ([], [Types.F64Type]))
];;

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
    Ast.ftype = as_phrase 0l;
    Ast.locals = [];
    (*Ast.body = [as_phrase (Ast.Const (as_phrase (Values.I32 42l)))];*)
    Ast.body = expf;
  }
;;

let get_func body = 
  {
    Ast.ftype = as_phrase 0l;
    Ast.locals = [];
    Ast.body = body;
  }
;;

let empty = get_module types_ [as_phrase (get_func (exp_to_func exp))] in
  let empty_module = as_phrase (empty) in
    let arrange_m = Arrange.module_ empty_module in
      Sexpr.print 80 arrange_m
;;

let wasm_to_file m = 
  let oc = open_out file_name in 
    Sexpr.output oc 80 m;
    close_out oc
;;

let arithmetic_ast =
  Test.make ~name:"Arithmetic expressions" ~count:10 
  (set_shrink tshrink arb_tree)
  (fun e -> 
    let empty = get_module types_ [as_phrase (get_func (exp_to_func e))] in
      let empty_module = as_phrase (empty) in
        let arrange_m = Arrange.module_ empty_module in
          wasm_to_file arrange_m
    ;
    Sys.command ("../script/compare.sh " ^ file_name) = 0
  )
;;

QCheck_runner.run_tests ~verbose:true [ arithmetic_ast; ] ;;