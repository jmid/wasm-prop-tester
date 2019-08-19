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

let get_func input ftype body =
  { Ast.ftype  = ftype;
    Ast.locals = input;
    Ast.body   = body; }

let module_to_wat m file =
  let m_sexpr = Arrange.module_ m in
  let oc = open_out file in
    Sexpr.output oc 80 m_sexpr;
    close_out oc

let module_to_wasm m file = 
  let s = Encode.encode m in
  let oc = open_out_bin file in
  try
    output_string oc s;
    close_out oc
  with exn -> close_out oc; raise exn

let context =
  { labels = [];
    locals = [];
    globals =
      { g_m_i32  = []; g_im_i32 = [];
        g_m_i64  = []; g_im_i64 = [];
        g_m_f32  = []; g_im_f32 = [];
        g_m_f64  = []; g_im_f64 = []; };
    funcs =
      { f_none = [];
        f_i32  = [(0, [])];
        f_i64  = [];
        f_f32  = [(1, [])];
        f_f64  = [(2, [])]; };
    imports = [];
    mems    = None;
    data    = [];
    return  = None;
    tables  = None;
    elems   = None;
    funcindex = 0; }

let max_number_args = 10
let max_number_funs = 10

(** value_type_opt_gen : value_type option **)
let value_type_opt_gen =
  Gen.(frequency
         [ 1, return None;
           3, map (fun t -> Some t) (oneofl [Helper.I32Type; Helper.I64Type; Helper.F32Type; Helper.F64Type])
         ])

(** stack_type_gen : int -> value_type list **)
let stack_type_gen n =
  Gen.(list_repeat n (oneofl [Helper.I32Type; Helper.I64Type; Helper.F32Type; Helper.F64Type] ))

(** func_type_gen : func_type **)
let func_type_gen =
  Gen.(int_bound max_number_args >>= fun n -> pair (stack_type_gen n) (value_type_opt_gen))

(** func_type_list_gen : func_type list **)
let func_type_list_gen = Gen.(list_size (int_bound max_number_funs) func_type_gen)

let process_funcs con flist index = 
  let rec process_flist funcs i = function
    | []      -> funcs
    | e::rst  -> 
      let funcs' = (match snd e with 
        | None          -> { funcs with f_none = (i, fst e)::funcs.f_none; }
        | Some I32Type  -> { funcs with f_i32  = (i, fst e)::funcs.f_i32; }
        | Some I64Type  -> { funcs with f_i64  = (i, fst e)::funcs.f_i64; }
        | Some F32Type  -> { funcs with f_f32  = (i, fst e)::funcs.f_f32; }
        | Some F64Type  -> { funcs with f_f64  = (i, fst e)::funcs.f_f64; }
        | Some _        -> funcs) in
      process_flist funcs' (i+1) rst in
  let funcs_ = process_flist con.funcs index flist in
  { con with funcs = funcs_; }

(** limits_gen : limits list **)
let limits_gen max_size = Gen.(
    let limit_gen =
      small_int >>= fun min ->
      int_range min max_size >>= fun max ->
      oneofl [ Some (Int32.of_int max); None; ] >>= fun max_opt ->
      let limits =
        { Types.min = Int32.of_int min;
          Types.max = max_opt; } in
      return (Some limits) in
    oneof [ limit_gen; return None;])

let limits_to_ast_memory = function
  | None   -> []
  | Some l -> [as_phrase { Ast.mtype = Types.MemoryType l }]

let limits_to_ast_table = function
  | None   -> []
  | Some l ->
    let table = { Ast.ttype = Types.TableType (l, Types.FuncRefType(*Types.AnyFuncType*)) } in
    [as_phrase table]

let data_segment_gen n = Gen.(
  oneofl [ Types.I32Type; Types.I64Type; Types.F32Type; Types.F64Type ] >>= fun t ->
    let size = Types.size t in
    pair string (int_bound (n - size)) >>= fun (s, i) ->
    return (as_phrase
              { Ast.index  = as_phrase (Int32.of_int 0);
                Ast.offset = as_phrase [ as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int i)))) ];
                Ast.init   = s; }))

let data_segment_list_gen = function
  | None   -> Gen.return []
  | Some (l: (Int32.t Types.limits)) ->
    let min = (Int32.to_int l.min) in
    if min > 0
    then Gen.(oneof [return []; list_size (int_bound min) (data_segment_gen (min * 65536))])
    else Gen.return []

let elem_segment_gen n m =
  Gen.(int_bound n >>= fun o ->
       let max_size = min (n - o) 10 in
       map (fun il -> (o, il)) (list_size (return max_size) (int_bound (m-1))))

let elems_to_ast_elems el = 
  List.map
    (fun (o, il) -> 
       let init = List.map (fun i -> as_phrase (Int32.of_int i)) il in
       as_phrase
         { Ast.index = as_phrase (Int32.of_int 0);
           Ast.offset = as_phrase [ as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int o)))) ];
           Ast.init = init; })
    el


let elem_segment_list_gen m = function
  | None -> Gen.return []
  | Some (l: (Int32.t Types.limits)) ->
    let min = Int32.to_int l.min in
    if min > 0
    then Gen.(let el_gen = list_size (int_bound min) (elem_segment_gen min m) in
              oneof [ return []; el_gen])
    else Gen.return []

(* as_phrase ({ Ast.gtype= Types.GlobalType (Types.I32Type, Immutable); Ast.value = as_phrase [] }) *)
let globals_gen con =
  let rec rec_glob_gen globals glist =
    Gen.(match glist with
        | []     -> return (List.rev globals)
        | t::rst ->
          let t_opt = Some t in
          let rules = [ (1, const_gen con t_opt 1); (*(11, getGlobal_gen con' t_opt 1);*) ] in
          Instr_gen.generate_rule rules >>= fun instrs_opt ->
          match instrs_opt with
          | None -> rec_glob_gen globals rst
          | Some (con'', inst, ts') ->
            oneofl [ Types.Immutable; Types.Mutable; ] >>= fun mutability ->
            rec_glob_gen ((t, mutability, inst)::globals) rst) in
  Gen.(list (oneofl [I32Type; I64Type; F32Type; F64Type]) >>= rec_glob_gen [])

let triple_to_global (t,m,inst) =
  as_phrase { Ast.gtype = Types.GlobalType (to_wasm_value_type t, m);
              Ast.value = as_phrase [inst] }

exception Type_not_expected of string;;

let process_globals con gtypelist =
  let rec rec_glob_process globals glist index =
    match glist with
      | []     -> globals
      | g::rst ->
        let nglobals (t, m, inst) =
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
        rec_glob_process (nglobals g) rst (index + 1) in
  { con with globals = rec_glob_process con.globals gtypelist 0 }

let context_gen =
  Gen.map
    (fun (mems, tables) ->
       { labels = [];
         locals = [];
         globals =
           { g_m_i32  = []; g_im_i32 = [];
             g_m_i64  = []; g_im_i64 = [];
             g_m_f32  = []; g_im_f32 = [];
             g_m_f64  = []; g_im_f64 = []; };
         funcs =
           { f_none = [(0, [I32Type]); (1, [F32Type]); (2, [F64Type]); (3, [])];
             f_i32 =  [(4, [])];
             f_i64 =  [];
             f_f32 =  [(5, [])];
             f_f64 =  [(6, [])]; };
         imports = [([I32Type], None); ([F32Type], None); ([F64Type], None);];
         mems = mems;
         data = [];
         return = None;
         tables = tables;
         elems = None;
         funcindex = 0; })
    (Gen.pair (limits_gen (int_of_float (2.0 ** 16.0))) (limits_gen 10000000))

let rec func_type_list_to_type_phrase func_type_list = match func_type_list with
  | []     -> []
  | e::rst ->
    let ot_opt = match snd e with
      | None   -> []
      | Some t -> [to_wasm_value_type t] in
    (as_phrase (Types.FuncType (to_stack_type (fst e), ot_opt)))::(func_type_list_to_type_phrase rst)

let process_elems (con: context_) el = 
  let rec elem_array elems i = function
    | [] -> elems
    | findex::rst  -> 
      let ftype_opt = get_ftype con findex in 
      let elems' = match ftype_opt with 
        | None -> elems
        | Some ftype  -> elems.(i) <- Some (snd ftype, findex); elems in
      elem_array elems' (i + 1) rst in
  let rec set_up elems = function
    | [] -> elems
    | e::rst  -> set_up (elem_array elems (fst e) (snd e)) rst in
  let size = match con.tables with
      | None -> 0
      | Some (l: Int32.t Types.limits) -> Int32.to_int l.min in
  let elems' = Array.make size None in
  { con with elems = Some (set_up elems' el); }

let context_with_ftype con funcindex =
  (* let ftype = List.nth con.funcs funcindex *)
  let ftype = match get_ftype con funcindex with
    | Some e -> e
    | _      -> raise (Type_not_expected (string_of_int funcindex)) in
  let label = [snd ftype, snd ftype] in (* Q: Why snd ftype twice? *)
  { con with 
    labels = label;
    locals = fst ftype;
    return = snd ftype;
    funcindex = funcindex }

let extend_context con data' = { con with data = data'; }

(* rec_func_gen : (Types.stack_type * Types.stack_type) list -> ((instr list) option) list Gen.t *)
let rec rec_func_gen con res func_types index = Gen.(match func_types with
    | []     -> return res
    | e::rst ->
      let func_t = match snd e with
        | Some t -> [t]
        | None   -> [] in
      Instr_gen.instr_gen (context_with_ftype con index) (fst e, func_t) >>= fun instrs_opt ->
      let instrs = match instrs_opt with
        | Some inst -> inst
        | None      -> [] in
      let func = as_phrase (get_func (to_stack_type (fst e)) (as_phrase (Int32.of_int index)) (List.rev instrs)) in
      rec_func_gen con (func::res) rst (index + 1)
  )

let module_gen = Gen.(
  let intypes = [([I32Type], None); ([F32Type], None); ([F64Type], None)] in
  let outtypes = [([], None); ([], Some (I32Type)); ([], Some (F32Type)); ([], Some (F64Type))] in

  oneofl [ None; Some (as_phrase 3l);] >>= fun start ->
  context_gen >>= fun context ->
  triple
    (globals_gen context) (func_type_list_gen) (data_segment_list_gen context.mems) >>= 
        fun (gltypelist, ftypelist, ds) ->
          (elem_segment_list_gen ((List.length ftypelist) + 7) context.tables) >>= 
          fun es ->

          let con = extend_context context ds in
          let con = process_globals con gltypelist in
          let con = process_funcs con ftypelist 7 in
          let con = process_elems con es in
          let ftypelist' = outtypes@ftypelist in
 
            rec_func_gen con [] ftypelist' 3 >>= fun funcs ->
                let imports_str = string_to_name "imports" in
                let log_str     = string_to_name "log" in
                let logfl_str   = string_to_name "logfl" in
                let mk_func_import s i =
                  as_phrase { Ast.module_name = imports_str;
                              Ast.item_name = s;
                              Ast.idesc = as_phrase (Ast.FuncImport (as_phrase i)) } in
                let mk_func_export s i =
                  as_phrase { Ast.name = string_to_name s;
                              Ast.edesc = as_phrase (Ast.FuncExport (as_phrase i)) } in
                return (as_phrase {
                    Ast.types = func_type_list_to_type_phrase (intypes@ftypelist');
                    Ast.globals  = List.map triple_to_global gltypelist;
                    Ast.tables   = limits_to_ast_table con.tables;
                    Ast.memories = limits_to_ast_memory con.mems;
                    Ast.funcs = List.rev funcs;
                    Ast.start = start;
                    Ast.elems = elems_to_ast_elems es;
                    Ast.data  = con.data;
                    Ast.imports =
                      [ mk_func_import log_str 0l;
                        mk_func_import logfl_str 1l;
                        mk_func_import logfl_str 2l; ];
                    Ast.exports =
                      [ mk_func_export "runi32" 4l;
                        mk_func_export "runf32" 5l;
                        mk_func_export "runf64" 6l; ];
                  })
)

let module_printer (m : Wasm.Ast.module_' Wasm.Source.phrase) =
  Sexpr.to_string 80 (Arrange.module_ m)

let i32_shrink i =
  if i = I32.zero then Iter.empty else Iter.return I32.zero
(*  let c = (I32.div_s i (I32.of_int_s 2)) in
    if c = i then Iter.empty else Iter.return c *)

let i64_shrink i =
  if i = I64.zero then Iter.empty else Iter.return I64.zero
(*  let c = (I64.div_s i (I64.of_int_s 2)) in
    if c = i then Iter.empty else Iter.return c *)

let f32_shrink i =
  if i = F32.zero then Iter.empty else Iter.return F32.zero

let f64_shrink i =
  if i = F64.zero then Iter.empty else Iter.return F64.zero

let rec max_global il = match il with
  | [] -> 0l
  | (e: Ast.instr)::rst ->
    match e.it with
    | Ast.Block (_,l) -> max (max_global rst) (max_global l)
    | Ast.Loop (_,l)  -> max (max_global rst) (max_global l)
    | Ast.If (_,l,l') -> max (max_global rst) (max (max_global l) (max_global l'))
    | Ast.GlobalGet v -> max v.it (max_global rst)
    | Ast.GlobalSet v -> max v.it (max_global rst)
    | _               -> max_global rst

let rec instr_list_shrink gs is = match is with
  | [] -> Iter.empty
  | i::is ->
    Iter.(
      (match is with
       | [] -> Iter.empty
       | j::is -> (match i.Source.it,j.Source.it with
           | Ast.Const _    , Ast.BrIf _
           | Ast.LocalGet _ , Ast.BrIf _
           | Ast.GlobalGet _, Ast.BrIf _
           | Ast.Const _    , Ast.Drop
           | Ast.LocalGet _ , Ast.Drop
           | Ast.GlobalGet _, Ast.Drop
           | Ast.Const _, Ast.LocalSet _
           | Ast.Const _, Ast.GlobalSet _
           | Ast.GlobalGet _, Ast.GlobalSet _
           | Ast.LocalGet _ , Ast.LocalSet _
           | Ast.GlobalGet _, Ast.LocalSet _
           | Ast.LocalGet _ , Ast.GlobalSet _ -> return is
           | Ast.Const _    , Ast.If (_,is1,is2)
           | Ast.LocalGet _ , Ast.If (_,is1,is2)
           | Ast.GlobalGet _, Ast.If (_,is1,is2) -> of_list [is1@is; is2@is]
           | _, _ -> empty))
      <+>
      (match i.Source.it with
       | Ast.Nop
       | Ast.LocalTee _ -> return is         (* no change in stack -> omit *)
       | Ast.GlobalSet g ->                  (* change GlobalSets into Drop *)
         return ((as_phrase Ast.Drop)::is)
       | Ast.GlobalGet g ->                  (* change GlobalGets into Consts *)
         let glob = List.nth gs (Int32.to_int g.it) in
         let zero = (match glob.Source.it.Ast.gtype with
          | GlobalType (I32Type,_) -> Values.I32 I32.zero
          | GlobalType (I64Type,_) -> Values.I64 I64.zero
          | GlobalType (F32Type,_) -> Values.F32 F32.zero
          | GlobalType (F64Type,_) -> Values.F64 F64.zero) in
         return ((as_phrase (Ast.Const (as_phrase zero)))::is)
       | Ast.Return ->
         if is=[] then empty else return [i] (* delete instrs after return *)
       | Ast.Block (sts,[])  -> return is    (* remove empty block *)
       | Ast.Block (sts,is') ->
         map (fun is'' -> as_phrase (Ast.Block (sts,is''))::is) (instr_list_shrink gs is')
       | Ast.Loop (sts,[])  -> return is     (* remove empty loop *)
       | Ast.Loop (sts,is') ->
         map (fun is'' -> as_phrase (Ast.Loop (sts,is''))::is) (instr_list_shrink gs is')
       | Ast.If (sts,is1,is2) ->
         (map (fun is' -> as_phrase (Ast.If (sts,is',is2))::is) (instr_list_shrink gs is1))
         <+>
         (map (fun is' -> as_phrase (Ast.If (sts,is1,is'))::is) (instr_list_shrink gs is2))
       | Ast.BrTable (vs,v) ->
         map (fun vs' -> as_phrase (Ast.BrTable (vs',v))::is) (Shrink.list vs)
       | Ast.Const l -> (match l.Source.it with
           | Values.I32 i ->
             map (fun j -> as_phrase (Ast.Const (as_phrase (Values.I32 j)))::is) (i32_shrink i)
           | Values.I64 i ->
             map (fun j -> as_phrase (Ast.Const (as_phrase (Values.I64 j)))::is) (i64_shrink i)
           | Values.F32 f ->
             map (fun g -> as_phrase (Ast.Const (as_phrase (Values.F32 g)))::is) (f32_shrink f)
           | Values.F64 f ->
             map (fun g -> as_phrase (Ast.Const (as_phrase (Values.F64 g)))::is) (f64_shrink f)
         )
       | _ -> empty)
      <+>
      map (fun is' -> i::is') (instr_list_shrink gs is)
    )

let global_shrink gs g =
  let g' = g.Source.it in
  Iter.map
    (fun is -> as_phrase { g' with Ast.value = as_phrase is })
    (instr_list_shrink gs g'.Ast.value.Source.it)
(*
  let { Ast.gtype : Wasm.Types.global_type; Ast.value : const; } = g' in
  match List.find_opt (fun g -> g.Source.it.Ast.gtype ) gs with
  | None -> Iter.empty
  | Some g'' ->

    get_global_indexes g'.Ast.gtype 
*)

let split n xs =
  let rec walk n xs ys = match n,ys with
    | 0,_     -> List.rev xs, ys
    | _,[]    -> List.rev xs, ys
    | _,y::ys -> walk (n-1) (y::xs) ys in
  walk n [] xs

(* a size-preserving list-shrinker *)
(*
let rec shrink_list_elements ~shrink xs = match xs with
  | [] -> Iter.empty
  | x::xs ->
    Iter.(
      map (fun x' -> x'::xs) (shrink x)
      <+>
      map (fun xs -> x::xs) (shrink_list_elements ~shrink xs))
*)

let rec shrink_functions gs (fs : Ast.func list) (types : Wasm.Ast.type_ list) =
  match fs with
  | [] -> Iter.empty
  | f::fs ->
    Iter.(
      (match f.it.Ast.body with
         | []  -> empty (* don't shrink empty body *)
         | [i] -> empty (* or one instr body *)
         | _  ->
           map
             (fun body' -> (as_phrase { f.it with Ast.body = body' })::fs)
             ((let var = f.it.ftype.it in
              let typ = List.nth types (Int32.to_int var) in
              match typ.Source.it with
              | Types.FuncType ([],[]) ->  return []
              | Types.FuncType (_,[rt]) ->
                let last = match rt with
                    | I32Type -> Values.I32 I32.zero
                    | I64Type -> Values.I64 I64.zero
                    | F32Type -> Values.F32 F32.zero
                    | F64Type -> Values.F64 F64.zero in
                return [as_phrase (Ast.Const (as_phrase last))]
              | _ -> empty)
              <+>
              instr_list_shrink gs f.Source.it.Ast.body))
      <+>
      map (fun fs' -> f::fs') (shrink_functions gs fs types))

let shrink_data s =
  Iter.map (fun s' -> as_phrase {s.Source.it with Ast.init = s'})
     (Shrink.string s.Source.it.Ast.init)

let module_valid m = try Valid.check_module m; true with Valid.Invalid (_,_) -> false

let module_shrink (m : Wasm.Ast.module_' Wasm.Source.phrase) =
  Iter.(map (* shrink func bodies *)
          (fun funs' -> as_phrase { m.it with Ast.funcs = funs' })
          (shrink_functions m.it.Ast.globals m.it.Ast.funcs m.it.Ast.types)
        <+> (* remove unneeded funcs *)
        filter module_valid
          (let fixed,rest = split 7 m.it.Ast.funcs in (* 3 imports, start, 3 exports *)
           (map
              (fun rest' -> as_phrase { m.it with Ast.funcs = fixed@rest' })
              (Shrink.list rest)))
        <+> (* shrink data segment *)
        map (fun ds -> as_phrase { m.it with Ast.data = ds })
          (Shrink.list ~shrink:shrink_data m.it.Ast.data)
        <+> (* remove start *)
        (match m.it.Ast.start with
         | None   -> empty
         | Some _ -> return (as_phrase { m.it with Ast.start = None }))
        <+> (* reduce globals *)
        filter module_valid
          (map (fun gs -> as_phrase { m.it with Ast.globals = gs })
             (Shrink.list ~shrink:(global_shrink m.it.Ast.globals) m.it.Ast.globals))
        <+> (* reduce declared types *)
        filter module_valid
          (map (fun ts -> as_phrase { m.it with Ast.types = ts }) (Shrink.list m.it.Ast.types))
        <+> (* shrink elems *)
        filter module_valid
          (map (fun es -> as_phrase { m.it with Ast.elems = es }) (Shrink.list m.it.Ast.elems)))

let arb_module = make ~print:module_printer ~shrink:module_shrink module_gen

let run_test =
  let host_debug vs = match vs with
    | [v] -> Printf.printf "%s\n" (Values.string_of_value v); []
    | _ -> failwith (Printf.sprintf "host_debug called with %i arguments" (List.length vs)) in
  Test.make ~name:"ref interpret run" ~count:100
  arb_module
  (function m ->
     let debug0 = Func.alloc_host (Types.FuncType ([I32Type],[])) host_debug in
     let debug1 = Func.alloc_host (Types.FuncType ([F32Type],[])) host_debug in
     let debug2 = Func.alloc_host (Types.FuncType ([F64Type],[])) host_debug in
     (*     try *)
       let modinst =
         Eval.init m [Instance.ExternFunc debug0;
                      Instance.ExternFunc debug1;
                      Instance.ExternFunc debug2] in
       ignore(Eval.invoke (AstFunc (Types.FuncType ([],[]),
                                    ref modinst,
                                    List.hd m.Source.it.Ast.funcs)) []);
       true
(*     with (Error (_,"unreachable executed")) -> true
         | _ -> false *)
  )

let conversion_test =
  Test.make ~name:"Conversion" ~count:1000
  arb_module
  (function m ->
    module_to_wat m wat_file_name;
    module_to_wasm m wasm_file_name;
    Sys.command ("../script/compare_refint.sh " ^ wat_file_name ^ " " ^ wasm_file_name) = 0)

let wabt_test =
  Test.make ~name:"wabt" ~count:1000
  arb_module
  (function m ->
    module_to_wat m wat_file_name;
    module_to_wasm m wasm_file_name;
    Sys.command ("../script/compare_wabt.sh " ^ wat_file_name ^ " " ^ wasm_file_name) = 0)

let float_test =
  Test.make ~name:"Round" ~count:1000
  float
  (function f ->
    let m = as_phrase
      { Ast.empty_module with
        Ast.types = func_type_list_to_type_phrase ([([], Some (F32Type))]);
        Ast.funcs = [as_phrase (get_func [] (as_phrase 0l)
                                  [as_phrase (Ast.Const (as_phrase (Values.F32 (F32.of_float 1.62315690127))))])]} in
    module_to_wat m wat_file_name;
    Sys.command ("../script/compare_engines.sh " ^ wat_file_name) = 0)

let buffer_test = 
  Test.make ~name:"Buffer" ~count:1000
  (make (Gen.return 2) 
    ~print:Print.int
    ~shrink:(fun i -> Iter.of_list [i * 100; i * 10; i * 2; i + 100; i + 50; i + 20; i + 5; i + 1;]))
  (function i ->
    let m = as_phrase
      { Ast.empty_module with
        Ast.types = func_type_list_to_type_phrase [([I32Type], None); ([I32Type], None); ([], Some I32Type)];
        Ast.funcs =
          [as_phrase (get_func [] (as_phrase 1l) [
               as_phrase (Ast.LocalGet (as_phrase 0l));
               as_phrase (Ast.Const (as_phrase (Values.I32 (Int32.of_int i))));
               as_phrase (Ast.Compare (Values.I32 (Ast.IntOp.Eq)));
               as_phrase (Ast.If ([],
                                  [as_phrase Ast.Return],
                                  [as_phrase (Ast.Const (as_phrase (Values.I32 1l)));
                                   as_phrase (Ast.Call (as_phrase 0l));
                                   as_phrase (Ast.LocalGet (as_phrase 0l));
                                   as_phrase (Ast.Const (as_phrase (Values.I32 1l)));
                                   as_phrase (Ast.Binary (Values.I32 Ast.IntOp.Add));
                                   as_phrase (Ast.Call (as_phrase 1l)) ]))]);
           as_phrase (get_func [] (as_phrase 2l) [
               as_phrase (Ast.Const (as_phrase (Values.I32 1l)));
               as_phrase (Ast.Call (as_phrase 1l));
               as_phrase (Ast.Loop ([Types.I32Type], [
                    (as_phrase (Ast.Const (as_phrase (Values.I32 1l))));
                    (as_phrase (Ast.Br (as_phrase 0l)))
                  ]))])];
        Ast.start = None;
        Ast.elems = [];
        Ast.data = [];
        Ast.imports = [
          as_phrase { Ast.module_name = string_to_name "imports";
                      Ast.item_name = string_to_name "log";
                      Ast.idesc = as_phrase (Ast.FuncImport (as_phrase 0l)) }];
        Ast.exports = [
          as_phrase { Ast.name = string_to_name "runi32";
                      Ast.edesc = as_phrase (Ast.FuncExport (as_phrase 2l)) }] } in
    module_to_wat m wat_file_name;
    Sys.command ("../script/compare_buffer.sh " ^ wat_file_name ) = 0)

let stack_test = 
  Test.make ~name:"Stack" ~count:1000 int
    (function i -> Sys.command ("../script/compare_stack.sh ") = 0)
