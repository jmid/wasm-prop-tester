open Wasm
open QCheck
open Instr_gen
open Helper

let tmp_dir_name = "tmp";;

let tmp_dir = match Sys.file_exists tmp_dir_name with
  | true  -> ()
  | false -> Unix.mkdir tmp_dir_name 0o775;;

let wat_file_name = tmp_dir_name ^ "/" ^ "tmp_module.wat";;
let wasm_file_name = tmp_dir_name ^ "/" ^ "tmp_module.wasm";;

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

let module_to_wat m file =
  let m_sexpr = Arrange.module_ m in
  let oc = open_out file in
    Sexpr.output oc 80 m_sexpr;
    close_out oc
;;

let module_to_wasm m file = 
  let s = Encode.encode (m) in
  let oc = open_out_bin file in
  try
    output_string oc s;
    close_out oc
  with exn -> close_out oc; raise exn
;;

let context = {
  labels = [];
  locals = [];
  globals = {
    g_m_i32 = [];
    g_im_i32 = [];
    g_m_i64 = [];
    g_im_i64 = [];
    g_m_f32 = [];
    g_im_f32 = [];
    g_m_f64 = [];
    g_im_f64 = [];
  };
  funcs = {
    f_none = [];
    f_i32 = [(0, [])];
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
  ])
;;

(** stack_type_gen : int -> value_type list **)
let stack_type_gen n = Gen.(list_repeat n (oneofl [Helper.I32Type; Helper.I64Type; Helper.F32Type; Helper.F64Type] ))
;;

(** func_type_gen : func_type **)
let func_type_gen = Gen.(int_bound 10 >>= fun n ->
  pair (stack_type_gen n) (value_type_opt_gen))
;;

(** func_type_list_gen : func_type list **)
let func_type_list_gen = Gen.(list_size (int_bound 10) func_type_gen)
;;

let process_funcs con flist index = 
  let rec process_flist funcs i = function
    | []      -> funcs
    | e::rst  -> 
      (let funcs' = (match snd e with 
        | None          -> { funcs with f_none = (i, fst e)::funcs.f_none; }
        | Some I32Type  -> { funcs with f_i32  = (i, fst e)::funcs.f_i32; }
        | Some I64Type  -> { funcs with f_i64  = (i, fst e)::funcs.f_i64; }
        | Some F32Type  -> { funcs with f_f32  = (i, fst e)::funcs.f_f32; }
        | Some F64Type  -> { funcs with f_f64  = (i, fst e)::funcs.f_f64; }
        | Some _        -> funcs
        ) in
        process_flist funcs' (i+1) rst
      ) in    
      let funcs_ = process_flist con.funcs index flist in
        { con with funcs = funcs_; }

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
    [as_phrase memory]

let limits_to_ast_table = function
  | None   -> []
  | Some l ->
    let table = {
      Ast.ttype = Types.TableType (l, Types.FuncRefType(*Types.AnyFuncType*))
    } in
    [as_phrase table]

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
        oneof [ return []; return dl]
    )
    else Gen.return []

let elem_segment_gen n m = Gen.(
  int_bound n >>= fun o ->
    let max_size = 
      if (n - o) >= 10 
      then 10
      else (n - o) 
    in
    list_size (return max_size) (int_bound (m-1)) >>= fun il ->
      return (o, il)
)

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
      let el_gen = list_size (int_bound min) (elem_segment_gen min m ) in
        oneof [ return []; el_gen]
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
  Gen.( list (oneofl [I32Type; I64Type; F32Type; F64Type]) >>= fun tlist ->
    let triples = [] in
    rec_glob_gen con triples tlist >>= fun globals -> return globals
  )

let triple_to_global = function (t, m, inst) ->
  (as_phrase ({ Ast.gtype = Types.GlobalType (to_wasm_value_type (t), m); Ast.value = as_phrase [inst] }))

exception Type_not_expected of string;;

let process_globals con gtypelist =
  let rec rec_glob_process globals glist index =
    match glist with
      | g::rst ->
        let nglobals = function (t, m, inst) ->
          (match t with
            | I32Type -> (match m with 
              | Types.Mutable   -> { globals with g_m_i32  = index::globals.g_m_i32; }
              | Types.Immutable -> { globals with g_im_i32 = index::globals.g_im_i32; }
              )
            | I64Type ->(match m with 
              | Types.Mutable   -> { globals with g_m_i64  = index::globals.g_m_i64; }
              | Types.Immutable -> { globals with g_im_i64 = index::globals.g_im_i64; }
              )
            | F32Type -> (match m with
              | Types.Mutable   -> { globals with g_m_f32  = index::globals.g_m_f32; }
              | Types.Immutable -> { globals with g_im_f32 = index::globals.g_im_f32; }
              )
            | F64Type ->(match m with 
              | Types.Mutable   -> { globals with g_m_f64  = index::globals.g_m_f64; }
              | Types.Immutable -> { globals with g_im_f64 = index::globals.g_im_f64; }
              )
            | _ -> raise (Type_not_expected "")
            ) in
        rec_glob_process (nglobals g) rst (index + 1)
      | []     -> globals
  in
  let globals' = rec_glob_process con.globals gtypelist 0 in
  { con with globals = globals'; }

let context_gen =
  Gen.(
    map
    (fun (mems, tables) ->
    {
      labels = [];
      locals = [];
      globals = {
        g_m_i32 = [];
        g_im_i32 = [];
        g_m_i64 = [];
        g_im_i64 = [];
        g_m_f32 = [];
        g_im_f32 = [];
        g_m_f64 = [];
        g_im_f64 = [];
      };
      funcs = {
        f_none = [(0, [I32Type]); (1, [F32Type]); (2, [F64Type]); (3, [])];
        f_i32 =  [(4, [])];
        f_i64 =  [];
        f_f32 =  [(5, [])];
        f_f64 =  [(6, [])];
      };
      imports = [([I32Type], None); ([F32Type], None); ([F64Type], None);];
      mems = mems;
      data = [];
      return = None;
      tables = tables;
      elems = None;
      funcindex = 0;
    })
    (pair (limits_gen (int_of_float (2.0 ** 16.0))) (limits_gen 10000000))
  )

let rec func_type_list_to_type_phrase func_type_list = match func_type_list with
  | e::rst -> let ot_opt = match (snd e) with
    | Some t -> [to_wasm_value_type t]
    | None   -> [] in
      (as_phrase (Types.FuncType (to_stack_type (fst e), ot_opt)))::(func_type_list_to_type_phrase rst)
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
  let elems' = Array.make size None in
  { con with elems = Some (set_up elems' el); }

let context_with_ftype con funcindex =
  (* let ftype = List.nth con.funcs funcindex *)
  let ftype = match get_ftype con funcindex with
    | Some e -> e
    | _      -> raise (Type_not_expected (string_of_int funcindex))
  in
    let label = [snd ftype, snd ftype] in
      { con with 
        labels = label;
        locals = fst ftype;
        return = snd ftype;
        funcindex = funcindex;
      }

let extend_context con data' =
  { con with data = data'; }

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
                  let func = as_phrase (get_func (to_stack_type (fst e)) (as_phrase (Int32.of_int index)) (List.rev instrs)) in
                    rec_func_gen con (func::res) rst (index + 1)
                )
              )
  | []     -> return res)

let module_gen = Gen.(
  let intypes = [([I32Type], None); ([F32Type], None); ([F64Type], None)] in
  let outtypes = [([], None); ([], Some (I32Type)); ([], Some (F32Type)); ([], Some (F64Type))] in

  oneofl [ None; Some (as_phrase 3l);] >>= fun start ->

  context_gen >>= fun context ->

    triple (globals_gen context) 
      (func_type_list_gen) 
      (data_segment_list_gen context.mems) >>= 
        fun (gltypelist, ftypelist, ds) ->
          (elem_segment_list_gen ((List.length (ftypelist)) + 7) context.tables) >>= 
          fun es ->

          let con = extend_context context ds in
          let con' = process_globals con gltypelist in
          let con'' = process_funcs con' ftypelist 7 in
          let con''' = process_elems con'' es in
            
            rec_func_gen con''' [] (outtypes@ftypelist) 3 >>= fun funcs ->
                    
                return (as_phrase {
                  Ast.types = (func_type_list_to_type_phrase (intypes@outtypes@ftypelist));
                  Ast.globals = (List.map triple_to_global gltypelist);
                  Ast.tables = (limits_to_ast_table con'''.tables);
                  Ast.memories = (limits_to_ast_memory con'''.mems);
                  Ast.funcs = (List.rev funcs);
                  Ast.start = start;
                  Ast.elems  = (elems_to_ast_elems es);
                  Ast.data = (con'''.data);
                  Ast.imports = [
                    as_phrase({
                      Ast.module_name = string_to_name "imports";
                      Ast.item_name = string_to_name "log";
                      Ast.idesc = as_phrase (Ast.FuncImport (as_phrase 0l))
                    });
                    as_phrase({
                      Ast.module_name = string_to_name "imports";
                      Ast.item_name = string_to_name "log";
                      Ast.idesc = as_phrase (Ast.FuncImport (as_phrase 1l))
                    });
                    as_phrase({
                      Ast.module_name = string_to_name "imports";
                      Ast.item_name = string_to_name "log";
                      Ast.idesc = as_phrase (Ast.FuncImport (as_phrase 2l))
                    });
                  ];
                  Ast.exports = [
                    as_phrase ({
                      Ast.name = string_to_name "runi32";
                      Ast.edesc = as_phrase (Ast.FuncExport (as_phrase 4l));
                    });
                    as_phrase ({
                      Ast.name = string_to_name "runf32";
                      Ast.edesc = as_phrase (Ast.FuncExport (as_phrase 5l));
                    });
                    as_phrase ({
                      Ast.name = string_to_name "runf64";
                      Ast.edesc = as_phrase (Ast.FuncExport (as_phrase 6l));
                    })
                  ];
                  }
                )
)

let arb_module = make module_gen

let stat_dir_name = "stat";;

let timestamp = string_of_float (Unix.time())

let stat_dir = match Sys.file_exists stat_dir_name with
  | true  -> ()
  | false -> Unix.mkdir stat_dir_name 0o775;;

let stat_file_name = stat_dir_name ^ "/" ^ timestamp ^ "stat." ^ "csv";;

let funcs_number (m: Ast.module_) = List.length m.it.funcs;;

let rec instrs_length il s = match il with
    | (e: Ast.instr)::rst  -> (match e.it with
        | Ast.Block (t, l)   -> (instrs_length rst (s)) + (instrs_length l (s))
        | Ast.Loop (t, l)    -> (instrs_length rst (s)) + (instrs_length l (s))
        | Ast.If (t, l1, l2) -> (instrs_length rst (s)) + (instrs_length l1 (s)) + (instrs_length l2 (s))
        | _                  -> (instrs_length rst (s)) + 1
      )
    | []      -> s

let call_func_length (m: Ast.module_) = 
  let f = List.nth m.it.funcs 1 in
  instrs_length f.it.body 0

let funcs_lenght (m: Ast.module_) = 
  List.fold_left (fun s (f: Ast.func) -> s + (instrs_length f.it.body 0)) 0 m.it.funcs

let all_func_length (m: Ast.module_) = 
  List.fold_left (fun s (f: Ast.func) -> s + (List.length f.it.body)) 0 m.it.funcs    

let elems_length (m: Ast.module_) = List.length m.it.elems

let globals_length (m: Ast.module_) = List.length m.it.globals

let data_length (m: Ast.module_) = List.length m.it.data

let rec call_number il s = match il with
  | (e: Ast.instr)::rst  -> (match e.it with
      | Ast.Call _   -> (call_number rst (s)) + 1
      | _            -> (call_number rst (s))
    )
  | []      -> s

let calls_number (m: Ast.module_) = 
    List.fold_left (fun s (f: Ast.func) -> s + (call_number f.it.body 0)) 0 m.it.funcs

let rec callIndirect_number il s = match il with
  | (e: Ast.instr)::rst  -> (match e.it with
      | Ast.CallIndirect _   -> (callIndirect_number rst (s)) + 1
      | _                    -> (callIndirect_number rst (s))
    )
  | []      -> s

let callIndirects_number (m: Ast.module_) = 
    List.fold_left (fun s (f: Ast.func) -> s + (callIndirect_number f.it.body 0)) 0 m.it.funcs
    
  

let stat_to_file m = 
  let oc = open_out_gen [Open_append; Open_creat] 0o666 stat_file_name in
    Printf.fprintf oc "%d;%d;%d;%d;%d;%d;%d;%d\n" (funcs_number m) (call_func_length m) (funcs_lenght m) (elems_length m) (globals_length m) (data_length m) (calls_number m) (callIndirects_number m);
    close_out oc;;

let implementations_test =
  Test.make ~name:"Implementations" ~count:10000
  arb_module
  (function m ->
    module_to_wat m wat_file_name;
    Sys.command ("../script/compare_engines.sh " ^ wat_file_name) = 0
  )
;;

let implementations_stat_test =
  Test.make ~name:"Stats" ~count:1000
  arb_module
  (function m ->
    stat_to_file m;
    module_to_wat m wat_file_name;
    Sys.command ("../script/compare_engines.sh " ^ wat_file_name ^ " " ^ timestamp) = 0
  )
;;

let conversion_test =
  Test.make ~name:"Conversion" ~count:1000
  arb_module
  (function m ->
    module_to_wat m wat_file_name;
    module_to_wasm m wasm_file_name;
    Sys.command ("../script/compare_refint.sh " ^ wat_file_name ^ " " ^ wasm_file_name) = 0
  )
;;

let wabt_test =
  Test.make ~name:"wabt" ~count:1000
  arb_module
  (function m ->
    module_to_wat m wat_file_name;
    module_to_wasm m wasm_file_name;
    Sys.command ("../script/compare_wabt.sh " ^ wat_file_name ^ " " ^ wasm_file_name) = 0
  )
;;

let float_test = 
  Test.make ~name:"Round" ~count:1000
  float
  (function f ->
    let m = (as_phrase
      { Ast.empty_module with
        Ast.types = (func_type_list_to_type_phrase ([([], Some (F32Type))]));
        Ast.funcs = [as_phrase (get_func [] (as_phrase 0l) [(as_phrase (Ast.Const (as_phrase (Values.F32 (F32.of_float 1.62315690127)))))])]
      }
    ) in
    module_to_wat m wat_file_name;
    Sys.command ("../script/compare_engines.sh " ^ wat_file_name ) = 0
  )
;;

let buffer_test = 
  Test.make ~name:"Buffer" ~count:1000
  (make (Gen.return 2) 
    ~print:Print.int
    ~shrink:(fun i -> (Iter.of_list [i * 100; i * 10; i * 2; i + 100; i + 50; i + 20; i + 5; i + 1;]))
  )
  (function i ->
    let m = as_phrase
      { Ast.empty_module with
      Ast.types = (func_type_list_to_type_phrase ([([I32Type], None);([I32Type], None);([], Some (I32Type))]));
      Ast.funcs = [as_phrase (get_func [] (as_phrase 1l) [
        (as_phrase (Ast.LocalGet (as_phrase 0l)));
        (as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int i)))));
        (as_phrase (Ast.Compare (Values.I32 (Ast.IntOp.Eq))));
        (as_phrase (Ast.If ([], (
          [
            (as_phrase Ast.Return)
          ]
          ), (
          [
            (as_phrase (Ast.Const (as_phrase (Values.I32 1l))));
            (as_phrase (Ast.Call (as_phrase 0l)));
            (as_phrase (Ast.LocalGet (as_phrase 0l)));
            (as_phrase (Ast.Const (as_phrase (Values.I32 1l))));
            (as_phrase (Ast.Binary (Values.I32 (Ast.IntOp.Add))));
            (as_phrase (Ast.Call (as_phrase 1l)));
          ]
          ))));
        ]);
      as_phrase (get_func [] (as_phrase 2l) [
        (as_phrase (Ast.Const (as_phrase (Values.I32 1l))));
        (as_phrase (Ast.Call (as_phrase 1l)));
        (as_phrase (Ast.Loop ([Types.I32Type], [
          (as_phrase (Ast.Const (as_phrase (Values.I32 1l))));
          (as_phrase (Ast.Br (as_phrase 0l)))
        ])))
        ]);
      ];
      Ast.start = None;
      Ast.elems = [];
      Ast.data = [];
      Ast.imports = [
        as_phrase({
          Ast.module_name = string_to_name "imports";
          Ast.item_name = string_to_name "log";
          Ast.idesc = as_phrase (Ast.FuncImport (as_phrase 0l))
        });
      ];
      Ast.exports = [
        as_phrase ({
          Ast.name = string_to_name "runi32";
          Ast.edesc = as_phrase (Ast.FuncExport (as_phrase 2l));
        });
      ];
    } in
    module_to_wat m wat_file_name;
    Sys.command ("../script/compare_buffer.sh " ^ wat_file_name ) = 0
  )
;;

let stack_test = 
  Test.make ~name:"Stack" ~count:1000
  int
  (function i ->
    Sys.command ("../script/compare_stack.sh ") = 0
  )
;;

(* QCheck_runner.set_seed(314195386);; *)

(* QCheck_runner.set_seed(516953645);; *)

(* QCheck_runner.set_seed(68061653);; *)

(* QCheck_runner.set_seed(36371916);; *)

(* QCheck_runner.set_seed(149512434);; *)

(* QCheck_runner.set_seed(498103647);; *)

(* QCheck_runner.set_seed(421117913);; *)
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
QCheck_runner.run_tests ~verbose:true [ implementations_test; (*implementation_test;*) (*implementation_stat_test;*) (*conversion_test; wabt_test;*) ] ;;

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

(* let l = {
          Types.min = Int32.of_int 10000;
          Types.max = None;
        };;
let table = {
      Ast.ttype = Types.TableType (l, Types.AnyFuncType)
    };;
let tables = [Helper.as_phrase table];;

let get_empty_module elems = 
  {
    Ast.types = [
      as_phrase (Types.FuncType ([], []));
      as_phrase (Types.FuncType ([], []));
      as_phrase (Types.FuncType ([], []));
      as_phrase (Types.FuncType ([], []));
      as_phrase (Types.FuncType ([], []));
    ];
    Ast.globals = [];
    Ast.tables = tables;
    Ast.memories = [];
    Ast.funcs = [
      as_phrase (get_func [] (as_phrase (Int32.of_int 0)) []);
      as_phrase (get_func [] (as_phrase (Int32.of_int 1)) []);
      as_phrase (get_func [] (as_phrase (Int32.of_int 2)) []);
      as_phrase (get_func [] (as_phrase (Int32.of_int 3)) []);
      as_phrase (get_func [] (as_phrase (Int32.of_int 4)) []);
    ];
    Ast.start = None;
    Ast.elems  = elems;
    Ast.data = [];
    Ast.imports = [];
    Ast.exports = [];
  }
;;

let c = Gen.return 10000;;

let rec gener i m =
  Sys.command ("echo "^ string_of_int i);
  if (i * 10) >= m then
  true
  else (
    let es = Gen.generate1 (
      elem_segment_list_gen 5 (Some ({
        Types.min = Int32.of_int (i * 10);
        Types.max = None;
      }))) in
      let mod_ = (as_phrase (get_empty_module (elems_to_ast_elems es))) in
      stat_to_file mod_;
      let arrange_m = Arrange.module_ mod_ in
        wasm_to_file arrange_m
      ;
      Sys.command ("../script/compare.sh " ^ file_name ^ " " ^ timestamp);
  (* (Gen.(
    elem_segment_list_gen 5 (Some ({
      Types.min = Int32.of_int m;
      Types.max = None;
    })) ==> fun es ->
    let mod_ = get_empty_module (elems_to_ast_elems es) in
    stat_to_file mod_;
    let arrange_m = Arrange.module_ mod_ in
      wasm_to_file arrange_m
    ;
    Sys.command ("../script/compare.sh " ^ file_name ^ " " ^ timestamp)
  )); *)
    gener (i + 1) m);;

let arb_elems = make (elem_segment_list_gen 5 (Some (l)))

let speed_test =
  Test.make ~name:"Speed" ~count:1
  (make c)
  (function m ->
    gener 0 m
    (* Sys.command ("echo "^ string_of_int m); *)
    (* Sys.command ("echo "^ string_of_int (List.length m)); *)
    (* true *)
    (* stat_to_file m;
    let arrange_m = Arrange.module_ m in
      wasm_to_file arrange_m
    ;
    Sys.command ("../script/compare.sh " ^ file_name ^ " " ^ timestamp) = 0 *)
  )
;;
QCheck_runner.run_tests ~verbose:true [ speed_test; ] ;; *)
