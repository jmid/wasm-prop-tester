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
    g_i32 = [];
    g_i64 = [];
    g_f32 = [];
    g_f64 = [];
  };
  funcs = {
    f_none = [];
    f_i32 = [];
    f_i64 = [];
    f_f32 = [];
    f_f64 = [];
  };
  imports = [];
  mems = None;
  data = [];
  return = None;
  tables = None;
  elems = None;
  funcindex = 0;
}
;;

(** value_type_opt_gen : value_type option **)
let value_type_opt_gen = Gen.(
  frequency [
    1, return None;
    3, (oneofl [Helper.I32Type; Helper.I64Type; Helper.F32Type; Helper.F64Type] >>= fun t -> return (Some t))
  ] >>= fun t_opt -> return t_opt)
;;

(** stack_type_gen : int -> value_type list **)
let stack_type_gen n = Gen.(list_repeat n (oneofl [Helper.I32Type; Helper.I64Type; Helper.F32Type; Helper.F64Type] >>= fun t -> return t))
;;

(** func_type_gen : func_type **)
let func_type_gen = Gen.(int_bound 10 >>= fun n ->
  pair (stack_type_gen n) (value_type_opt_gen) >>= fun t -> return t)
;;

(** func_type_list_gen : func_type list **)
let func_type_list_gen = Gen.(list_size small_nat func_type_gen)
;;

(** func_type_list_gen : func_type list **)
let func_type_list_gen2 = Gen.(list_size (int_bound 2) func_type_gen)
;;

let process_funcs con flist = 
  let rec process_flist funcs i = function
    | []      -> funcs
    | e::rst  -> 
      (let funcs' = (match snd e with 
        | None    -> {
            f_none = (i, fst e)::funcs.f_none;
            f_i32  = funcs.f_i32;
            f_i64  = funcs.f_i64;
            f_f32  = funcs.f_f32;
            f_f64  = funcs.f_f64;
          }
        | Some I32Type    -> {
            f_none = funcs.f_none;
            f_i32  = (i, fst e)::funcs.f_i32;
            f_i64  = funcs.f_i64;
            f_f32  = funcs.f_f32;
            f_f64  = funcs.f_f64;
          }
        | Some I64Type    -> {
            f_none = funcs.f_none;
            f_i32  = funcs.f_i32;
            f_i64  = (i, fst e)::funcs.f_i64;
            f_f32  = funcs.f_f32;
            f_f64  = funcs.f_f64;
          }
        | Some F32Type    -> {
            f_none = funcs.f_none;
            f_i32  = funcs.f_i32;
            f_i64  = funcs.f_i64;
            f_f32  = (i, fst e)::funcs.f_f32;
            f_f64  = funcs.f_f64;
          }
        | Some F64Type    -> {
            f_none = funcs.f_none;
            f_i32  = funcs.f_i32;
            f_i64  = funcs.f_i64;
            f_f32  = funcs.f_f32;
            f_f64  = (i, fst e)::funcs.f_f64;
          }
        | Some _          -> 
          funcs
        ) in
        process_flist funcs' (i+1) rst
      ) in    
      let funcs = process_flist con.funcs 3 flist in
      {
        labels = con.labels;
        locals = con.locals;
        globals = con.globals;
        funcs = funcs;
        imports = con.imports;
        mems = con.mems;
        data = con.data;
        return = con.return;
        tables = con.tables;
        elems = con.elems;
        funcindex = con.funcindex;
      }

(** limits_gen : limits list **)
let limits_gen max_size = Gen.(
  let limit_gen = (small_int >>= fun min ->
    int_range min max_size >>= fun max ->
      oneofl [ Some (Int32.of_int max); None; ] >>= fun max_opt ->
        let limits = {
          Types.min = Int32.of_int min;
          Types.max = max_opt;
        } in
        return (Some limits))
  in
  oneof [ limit_gen; return None;])

let limits_to_ast_memory = function
  | None   -> []
  | Some l ->
    let memory = {
      Ast.mtype = Types.MemoryType l
    } in
    [Helper.as_phrase memory]

let limits_to_ast_table = function
  | None   -> []
  | Some l ->
    let table = {
      Ast.ttype = Types.TableType (l, Types.AnyFuncType)
    } in
    [Helper.as_phrase table]

(*
let rec ds_to_list l sol i = match sol with
  | (s, o)::rst ->
    let ds = Helper.as_phrase ({
      Ast.index = as_phrase (Int32.of_int i);
      Ast.offset = as_phrase [ as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int o)))) ];
      Ast.init = s;
    }) in
    ds_to_list (ds::l) rst (i + 1)
  | []     -> l

let data_segments_gen = function
  | None   -> Gen.return []
  | Some (l: (Int32.t Types.limits)) ->
    let min = (Int32.to_int l.min) in
    if min > 0
    then Gen.(
      oneofl [ Types.I32Type; Types.I64Type; Types.F32Type; Types.F64Type ] >>= fun t ->
        let size = Types.size t in
          list_size (int_bound min) (pair string (int_bound ((min * 65536) - size))) >>= fun sol ->
            return (List.rev (ds_to_list [] sol 0))
    )
    else Gen.return []
*)

let data_segment_gen n = Gen.(
  oneofl [ Types.I32Type; Types.I64Type; Types.F32Type; Types.F64Type ] >>= fun t ->
    let size = Types.size t in
    pair string (int_bound (n - size)) >>= fun (s, i) ->
      return (Helper.as_phrase ({
          Ast.index = as_phrase (Int32.of_int 0);
          Ast.offset = as_phrase [ as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int i)))) ];
          Ast.init = s;
        }))
)

let data_segment_list_gen = function
  | None   -> Gen.return []
  | Some (l: (Int32.t Types.limits)) ->
    let min = (Int32.to_int l.min) in
    if min > 0
    then Gen.(
      list_size (int_bound min) (data_segment_gen (min * 65536)) >>= fun dl ->
        return dl
    )
    else Gen.return []

let elem_segment_gen n m = Gen.(
  int_bound n >>= fun o ->
    list_size (int_bound (n - o)) (int_bound (m-1)) >>= fun il ->
      return (o, il)
      (* let init = List.map (fun i -> as_phrase (Int32.of_int i)) il in
        return (as_phrase ({
          Ast.index = as_phrase (Int32.of_int 0);
          Ast.offset = as_phrase [ as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int o)))) ];
          Ast.init = init;
        })) *)
)

(* let elem_segment_list_gen (con: context_) flist = Gen.(
  let elem_gen = (match con.tables with
    | None   -> return None
    | Some (l: (Int32.t Types.limits)) ->
      let min = (Int32.to_int l.min) in 
      if min > 0
      then (
        int_bound min >>= fun size ->
          let elems' = Array.make size None in
            return (Some elems')
      )
      else return None)
  in elem_gen >>= fun elems ->
  return
  {
    labels = con.labels;
    locals = con.locals;
    globals = con.globals;
    funcs = con.funcs;
    imports = con.imports;
    mems = con.mems;
    data = con.data;
    return = con.return;
    tables = con.tables;
    elems = elems;
    funcindex = con.funcindex;
  }) *)

let elems_to_ast_elems el = 
  List.( map (
    fun (o, il) -> 
      let init = map (fun i -> as_phrase (Int32.of_int i)) il in
        (as_phrase ({
          Ast.index = as_phrase (Int32.of_int 0);
          Ast.offset = as_phrase [ as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int o)))) ];
          Ast.init = init;
        }))
      )
    el
  ) 

let elem_segment_list_gen m = function
  | None   -> Gen.return []
  | Some (l: (Int32.t Types.limits)) ->
    let min = (Int32.to_int l.min) in
    if min > 0
    then Gen.(
      list_size (int_bound min) (elem_segment_gen min m ) >>= fun el ->
        return el
    )
  else Gen.return []


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
  Gen.( list (oneofl [Helper.I32Type; Helper.I64Type; Helper.F32Type; Helper.F64Type]) >>= fun tlist ->
    let triples = [] in
    rec_glob_gen con triples tlist >>= fun globals -> return globals
  )

let triple_to_global = function (t, m, inst) ->
  (Helper.as_phrase ({ Ast.gtype = Types.GlobalType (Helper.to_wasm_value_type (t), m); Ast.value = as_phrase [inst] }))

exception Type_not_expected of string;;

let process_globals con gtypelist =
  let rec rec_glob_process globals glist index =
    match glist with
      | g::rst ->
        let nglobals = function (t, m, inst) ->
            (match t with
              | Helper.I32Type ->
                {
                  g_i32 = globals.g_i32@[(index, m)];
                  g_i64 = globals.g_i64;
                  g_f32 = globals.g_f32;
                  g_f64 = globals.g_f64;
                }
              | Helper.I64Type ->
                {
                  g_i32 = globals.g_i32;
                  g_i64 = globals.g_i64@[(index, m)];
                  g_f32 = globals.g_f32;
                  g_f64 = globals.g_f64;
                }
              | Helper.F32Type ->
                {
                  g_i32 = globals.g_i32;
                  g_i64 = globals.g_i64;
                  g_f32 = globals.g_f32@[(index, m)];
                  g_f64 = globals.g_f64;
                }
              | Helper.F64Type ->
                {
                  g_i32 = globals.g_i32;
                  g_i64 = globals.g_i64;
                  g_f32 = globals.g_f32;
                  g_f64 = globals.g_f64@[(index, m)];
                }
              | _ -> raise (Type_not_expected "")
              ) in
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
    data = con.data;
    return = con.return;
    tables = con.tables;
    elems = con.elems;
    funcindex = con.funcindex;
  }

let context_gen =
  Gen.(
    pair (limits_gen (int_of_float (2.0 ** 16.0))) (limits_gen 10000000 (*int_of_float (2.0 ** 32.0)*)) >>= fun (mems, tables) ->
      return {
        labels = [];
        locals = [];
        globals = {
          g_i32 = [];
          g_i64 = [];
          g_f32 = [];
          g_f64 = [];
        };
        funcs = {
          f_none = [(0, [Helper.I32Type]); (1, [])];
          f_i32 =  [(2, [])];
          f_i64 =  [];
          f_f32 =  [];
          f_f64 =  [];
        };
        (* funcs = ([], None)::([], Some (Helper.I32Type))::funcs; *)
        imports = [([Helper.I32Type], None)];
        mems = mems;
        data = [];
        return = None;
        tables = tables;
        elems = None;
        funcindex = 0;
      }
  )

let rec func_type_list_to_type_phrase func_type_list = match func_type_list with
  | e::rst -> let ot_opt = match (snd e) with
    | Some t -> [Helper.to_wasm_value_type t]
    | None   -> [] in
      (as_phrase (Types.FuncType (Helper.to_stack_type (fst e), ot_opt)))::(func_type_list_to_type_phrase rst)
  | []     -> []

let process_elems (con: context_) el = 
  let rec elem_array elems i = function
    | []      -> elems
    | findex::rst  -> 
      let ftype_opt = get_ftype con findex in 
      let elems' = match ftype_opt with 
        | None        -> elems
        | Some ftype  -> elems.(i) <- Some (snd ftype, findex); elems in
      elem_array elems' (i + 1) rst in
  let rec set_up elems = function
    | []      -> elems
    | e::rst  -> set_up (elem_array elems (fst e) (snd e)) rst in
  let size = ( match con.tables with
      | None   -> 0
      | Some (l: (Int32.t Types.limits)) -> Int32.to_int l.min
    ) in
  let elems = Array.make size None in
  {
    labels = con.labels;
    locals = con.locals;
    globals = con.globals;
    funcs = con.funcs;
    imports = con.imports;
    mems = con.mems;
    data = con.data;
    return = con.return;
    tables = con.tables;
    elems = Some (set_up elems el);
    funcindex = con.funcindex;
  }

let context_with_ftype con funcindex =
  (* let ftype = List.nth con.funcs funcindex *)
  let ftype = match get_ftype con funcindex with
    | Some e -> e
    | _      -> raise (Type_not_expected (string_of_int funcindex))
  in
    let label = [snd ftype, snd ftype]
    in
      let con' = {
        labels = label;
        locals = fst ftype;
        globals = con.globals;
        funcs = con.funcs;
        imports = con.imports;
        mems = con.mems;
        data = con.data;
        return = snd ftype;
        tables = con.tables;
        elems = con.elems;
        funcindex = funcindex;
      } in
      con'

let extend_context con data =
  let con' = {
    labels = con.labels;
    locals = con.locals;
    globals = con.globals;
    funcs = con.funcs;
    imports = con.imports;
    mems = con.mems;
    data = data;
    return = None;
    tables = con.tables;
    elems = con.elems;
    funcindex = con.funcindex;
  } in
  con'

(* rec_func_gen : (Types.stack_type * Types.stack_type) list -> ((instr list) option) list Gen.t *)
let rec rec_func_gen con res func_types index = Gen.(match func_types with
  | e::rst -> let func_t = match snd e with
                | Some t -> [t]
                | None   -> []
              in
              (Instr_gen.instr_gen (context_with_ftype con index) (fst e, func_t) >>= fun instrs_opt ->
                (let instrs = match instrs_opt with
                    | Some inst -> inst
                    | None      -> []
                  in
                  let func = as_phrase (get_func (Helper.to_stack_type (fst e)) (as_phrase (Int32.of_int index)) (List.rev instrs)) in
                    rec_func_gen con (func::res) rst (index + 1)
                )
              )
  | []     -> return res)

let module_gen = Gen.(
  context_gen >>= fun context ->
    globals_gen context >>= fun gltypelist ->
      func_type_list_gen2 >>= fun ftypelist ->
        data_segment_list_gen context.mems >>= fun ds ->
          let con = extend_context context ds in
            let con' = process_globals con gltypelist in
              let con'' = process_funcs con' ftypelist in 
                elem_segment_list_gen (List.length (([Helper.I32Type], None)::([], None)::([], Some (Helper.I32Type))::ftypelist)) con''.tables >>= fun es ->
                  let con''' = process_elems con'' es in               
                    rec_func_gen con''' [] (([], None)::([], Some (Helper.I32Type))::ftypelist) 1 >>= fun funcs ->
                      return (as_phrase
                        (get_module
                          (func_type_list_to_type_phrase (([Helper.I32Type], None)::([], None)::([], Some (Helper.I32Type))::ftypelist))
                          (List.rev funcs)
                          (limits_to_ast_memory con'''.mems)
                          (List.map triple_to_global gltypelist)
                          con'''.data
                          (limits_to_ast_table con'''.tables)
                          (elems_to_ast_elems es)))
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

(* QCheck_runner.set_seed(31444403);; *)
(* QCheck_runner.set_seed(3606552);; *)
(* QCheck_runner.set_seed(243782223);; *)
(* QCheck_runner.set_seed(265188083);; *)
(* QCheck_runner.set_seed(196716697);; *)
(* QCheck_runner.set_seed(182324116);; *)
(* QCheck_runner.set_seed(15600868);; *)
(* QCheck_runner.set_seed(457392187);; *)
(* QCheck_runner.set_seed(416362809);; *)
(* QCheck_runner.set_seed(393719003);; *)
(* QCheck_runner.set_seed(294956219);; *)
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