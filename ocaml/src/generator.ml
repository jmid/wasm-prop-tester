open Wasm
open QCheck
open Instr_gen
open Helper

let tmp_dir_name = "tmp";;


let tmp_dir = match Sys.file_exists tmp_dir_name with
  | true  -> ()
  | false -> Unix.mkdir tmp_dir_name 0o775;;

let file_name = tmp_dir_name ^ "/" ^ "tmp_sexpr.wat";;

let string_to_name s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (Char.code(s.[i]) :: l) in
  exp (String.length s - 1) []
;;

let get_func input ftype body = 
  {
    Ast.ftype = ftype;
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
  locals = [];
  globals = {
    i32 = [];
    i64 = [];
    f32 = [];
    f64 = [];
  };
  funcs = [];
  imports = [];
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
let func_type_gen = Gen.(small_int >>= fun n -> 
  pair (stack_type_gen n) (value_type_opt_gen) >>= fun t -> return t)
;;

(** func_type_list_gen : func_type list **)
let func_type_list_gen = Gen.(list_size small_nat func_type_gen)
;;

(** func_type_list_gen : func_type list **)
let func_type_list_gen2 = Gen.(list_size (int_bound 2) func_type_gen)
;;

(** memory_gen : memory list **)
let memory_gen = Gen.( 
  let mem_gen = (small_int >>= fun min -> 
    int_range min 65536 >>= fun max ->
      oneofl [ Some (Int32.of_int max); None; ] >>= fun max_opt ->
        let limits = {
          Types.min = Int32.of_int min;
          Types.max = max_opt;
        } in
        let memory = {
          Ast.mtype = Types.MemoryType limits
        } in
        return [Helper.as_phrase memory])
  in
  oneof [ mem_gen; (*return [];*) ])

(* as_phrase ({ Ast.gtype= Types.GlobalType (Types.I32Type, Immutable); Ast.value = as_phrase [] }) *)
let globals_gen con =
  let rec rec_glob_gen con' globals glist = 
    Gen.(
    match glist with
      | t::rst -> let t_opt = Some (t) in
        let rules = [ (1, const_gen con' t_opt 1); (*(11, getGlobal_gen con' t_opt 1);*) ] in
        (Instr_gen.generate_rule rules >>= fun instrs_opt -> 
          (match instrs_opt with
            | Some (con'', inst, ts') -> 
              oneofl [ Types.Immutable; Types.Mutable; ] >>= fun mutability ->
                (rec_glob_gen con' (globals@[(t, mutability, inst)]) rst) 
            | None                    -> (rec_glob_gen con' globals rst) 
          ))
      | []     -> return globals )
  in
  Gen.( list (oneofl [Types.I32Type; Types.I64Type; Types.F32Type; Types.F64Type]) >>= fun tlist ->
    let triples = [] in
    rec_glob_gen con triples tlist >>= fun globals -> return globals
  )

let triple_to_global = function (t, m, inst) ->
  (Helper.as_phrase ({ Ast.gtype = Types.GlobalType (t, m); Ast.value = as_phrase [inst] }))

let process_globals con gtypelist =
  let rec rec_glob_process globals glist index = 
    match glist with
      | g::rst -> 
        let nglobals = function (t, m, inst) -> 
            (match t with
              | Types.I32Type -> 
                {
                  i32 = globals.i32@[(index, m)];
                  i64 = globals.i64;
                  f32 = globals.f32;
                  f64 = globals.f64;
                }
              | Types.I64Type -> 
                {
                  i32 = globals.i32;
                  i64 = globals.i64@[(index, m)];
                  f32 = globals.f32;
                  f64 = globals.f64;
                }
              | Types.F32Type -> 
                {
                  i32 = globals.i32;
                  i64 = globals.i64;
                  f32 = globals.f32@[(index, m)];
                  f64 = globals.f64;
                }
              | Types.F64Type -> 
                {
                  i32 = globals.i32;
                  i64 = globals.i64;
                  f32 = globals.f32;
                  f64 = globals.f64@[(index, m)];
                }) in
        rec_glob_process (nglobals g) rst (index + 1)
      | []     -> globals
  in
  let globals = rec_glob_process con.globals gtypelist 0 in
  {
    labels = con.labels;
    locals = con.locals;
    globals = globals;
    funcs = con.funcs;
    imports = con.imports;
    mems = con.mems;
    return = con.return;
    tables = con.tables;
    funcindex = con.funcindex;
  }

let context_gen = 
  Gen.(func_type_list_gen2 >>= fun funcs ->
    memory_gen >>= fun mems ->
      return {
        labels = [];
        locals = [];
        globals = {
          i32 = [];
          i64 = [];
          f32 = [];
          f64 = [];
        };
        funcs = ([], None)::([], Some (Types.I32Type))::funcs;
        imports = [([Types.I32Type], None)];
        mems = mems;
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
        imports = con.imports;
        mems = con.mems;
        return = None;
        tables = con.tables;
        funcindex = funcindex + 1;
      } in
      con'

let module_gen = Gen.(context_gen >>= fun context -> 
  globals_gen context >>= fun gltypelist ->
    let con = process_globals context gltypelist in
    (* rec_func_gen : (Types.stack_type * Types.stack_type) list -> ((instr list) option) list Gen.t *)
    let rec rec_func_gen res func_types index = match func_types with
      | e::rst -> let func_t = match snd e with
                    | Some t -> [t]
                    | None   -> []
                  in
                  (Instr_gen.instr_gen (context_with_ftype con index) (fst e, func_t) >>= fun instrs_opt -> 
                    (let instrs = match instrs_opt with
                        | Some inst -> inst
                        | None      -> [] 
                      in
                      let func = as_phrase (get_func (fst e) (as_phrase (Int32.of_int (index + 1))) (List.rev instrs)) in
                        rec_func_gen (func::res) rst (index + 1)
                    )
                  )
      | []     -> return res in
      rec_func_gen [] con.funcs 0 >>= fun funcs ->
        return (as_phrase (get_module (func_type_list_to_type_phrase (([Types.I32Type], None)::con.funcs)) (List.rev funcs) con.mems (List.map triple_to_global gltypelist)))
  )

let arb_module = make module_gen

let module_test =
  Test.make ~name:"Modules" ~count:1 
  arb_module
  (function m ->
    let arrange_m = Arrange.module_ m in
      wasm_to_file arrange_m
    ;
    Sys.command ("../script/compare.sh " ^ file_name) = 0
  )
;;

(* QCheck_runner.set_seed(457392187);; *)
(* QCheck_runner.set_seed(416362809);; *)
(* QCheck_runner.set_seed(393719003);; *)
QCheck_runner.set_seed(294956219);;
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
294956219
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