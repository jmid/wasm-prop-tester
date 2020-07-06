open Wasm
open QCheck
open Instr_gen
open Helper

let tmp_dir_name = "tmp"

let tmp_dir = match Sys.file_exists tmp_dir_name with
  | true  -> ()
  | false -> Unix.mkdir tmp_dir_name 0o775

let wat_file_name = tmp_dir_name ^ "/" ^ "tmp_module.wat"
let wasm_file_name = tmp_dir_name ^ "/" ^ "tmp_module.wasm"

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

(** func_type_gen : func_type **)
let func_type_gen =
  Gen.(int_bound max_number_args >>= fun n -> pair (stack_type_gen n) value_type_opt_gen)

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
          generate_rule rules >>= fun instrs_opt ->
          match instrs_opt with
          | None -> rec_glob_gen globals rst
          | Some (con'', inst, ts') ->
            oneofl [ Types.Immutable; Types.Mutable; ] >>= fun mutability ->
            rec_glob_gen ((t, mutability, inst)::globals) rst) in
  Gen.(list value_type_gen >>= rec_glob_gen [])

let triple_to_global (t,m,inst) =
  as_phrase { Ast.gtype = Types.GlobalType (to_wasm_value_type t, m);
              Ast.value = as_phrase [inst] }

exception Type_not_expected of string

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
    locals = fst ftype;  (* FIXME: only params, not an arbitrary number of locals *)
    return = snd ftype;
    funcindex = funcindex }

(* rec_func_gen : (Types.stack_type * Types.stack_type) list -> ((instr list) option) list Gen.t *)
let rec rec_func_gen con res func_types index = Gen.(match func_types with
    | []     -> return res
    | e::rst ->
      let func_t = match snd e with
        | Some t -> [t]
        | None   -> [] in
      instr_gen (context_with_ftype con index) (fst e, func_t) >>= fun instrs_opt ->
      let instrs = match instrs_opt with
        | Some inst -> inst
        | None      -> [] in (*FIXME: failed generation attempt turned into empty list*)
      let func = as_phrase (get_func (to_stack_type (fst e)) (as_phrase (Int32.of_int index)) (List.rev instrs)) in
      rec_func_gen con (func::res) rst (index + 1))

let module_gen = Gen.(
  let intypes = [([I32Type], None); ([F32Type], None); ([F64Type], None)] in
  let outtypes = [([], None); ([], Some (I32Type)); ([], Some (F32Type)); ([], Some (F64Type))] in

  oneofl [ None; Some (as_phrase 3l);] >>= fun start ->
  context_gen >>= fun context ->
  triple
    (globals_gen context) func_type_list_gen (data_segment_list_gen context.mems) >>=
        fun (gltypelist, ftypelist, ds) ->
          elem_segment_list_gen (List.length ftypelist + 7) context.tables >>=
          fun es ->

          let con = { context with data = ds } in
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

let arb_module = make ~print:module_printer ~shrink:Shrinker.module_shrink module_gen

let iter_exports apply m =
  let import_len = List.length m.Source.it.Ast.imports in
  let exports =
    List.fold_right (fun e acc -> match e.Source.it.Ast.name,e.Source.it.Ast.edesc.Source.it with
        | name, FuncExport e ->
          let i = Int32.to_int e.Source.it in
          let f = List.nth m.Source.it.funcs (i - import_len) in
          let str = Wasm.Ast.string_of_name name in
          (name,str,i,f)::acc
        | _, _ -> acc) m.Source.it.Ast.exports [] in
  let order = List.sort (fun (_,s,_,_) (_,s',_,_) -> String.compare s s') exports in
  List.iter (fun (name,str,i,f) -> apply str f) order

module JSLikeEval =
struct
  let tobits = function
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'a' -> "1010"
    | 'b' -> "1011"
    | 'c' -> "1100"
    | 'd' -> "1101"
    | 'e' -> "1110"
    | 'f' -> "1111"
    |  _  -> failwith "tobits given non hexadecimal char"

  (* Code adapted from Chet Murthy
       https://discuss.ocaml.org/t/read-write-floats-in-binary-representation/5044/9 *)
  (* works for non-negative, non-inf, non-nan f *)
  let to_string f =
    (let s = Printf.sprintf "%h" f in
     let s = String.sub s 2 ((String.length s) - 2) in
     let mant,exp = match String.split_on_char 'p' s with
       | [mant;exp] -> mant,exp
       | _ -> failwith "to_string: Split_on_char 'p' gave <> 2 results" in
     let exp = int_of_string exp in
     let mant,morexp = match String.split_on_char '.' mant with
       | [mant] -> mant,exp
       | [a;b] -> a^b, exp - 4 * (String.length b)
       | _ -> failwith "to_string: Split_on_char '.' gave <> 1,2 results" in
     let tobits_seq c = c |> tobits |> String.to_seq in
     let mantbits = mant |> String.to_seq |> Seq.flat_map tobits_seq |> String.of_seq in
     (* adjust for trailing zeros *)
     let mantbits, morexp = match String.rindex_opt mantbits '1' with
       | Some onepos when onepos < String.length mantbits - 1
         -> String.sub mantbits 0 (onepos+1), morexp + (String.length mantbits - (onepos+1))
       | _ -> mantbits, morexp in
     let fin =
       if morexp >= 0
       then mantbits ^ (String.make morexp '0') (*^ "."*)
       else
         let pad_mantbits =
           let mantlen = String.length mantbits in
           if mantlen >= -morexp
           then mantbits
           else (String.make ((-morexp)-mantlen) '0') ^ mantbits in
         let padmantlen = String.length pad_mantbits in
         (if 0 = padmantlen - -morexp
          then "0."
          else (String.sub pad_mantbits 0 (padmantlen - -morexp)) ^ ".")
           ^ (String.sub pad_mantbits (padmantlen - -morexp) (-morexp))
     in
     let finlen = String.length fin in
     let fin = match String.index_opt fin '1', String.index_opt fin '.' with
       | Some onepos, None ->                             (* strip leading zeros before 1 *)
         String.sub fin onepos (finlen - onepos)
       | Some onepos, Some dotpos when onepos < dotpos -> (* strip leading zeros before 1 *)
         String.sub fin onepos (finlen - onepos)
       | _, Some dotpos when dotpos > 1 -> (* dotpos < onepos, if present *)
         String.sub fin (dotpos - 1) (finlen - (dotpos - 1)) (* strip leading zeros before . *)
       | None, None -> "0"
       | _ -> fin
     in fin)

  let f32_to_string f =
    let x = F32.to_bits f in
    let is_nan f = F32.ne f f in
    let abs x = Int32.logand x Int32.max_int in
    let is_inf x =
      let pos_inf = Int32.bits_of_float (1.0 /. 0.0) in
      let neg_inf = Int32.bits_of_float (-. (1.0 /. 0.0)) in
      x = pos_inf || x = neg_inf in
    (if F32.lt f F32.zero then "-" else "") ^
    if is_nan f then "NaN" else
    if is_inf x then "Infinity" else
      let x = abs x in (* non-neg, non-inf, non-nan case *)
      let f = F32.to_float (F32.of_bits x) in
      to_string f

  let f64_to_string f =
    let x = F64.to_bits f in
    let is_nan f = F64.ne f f in
    let abs x = Int64.logand x Int64.max_int in
    let is_inf x =
      let pos_inf = Int64.bits_of_float (1.0 /. 0.0) in
      let neg_inf = Int64.bits_of_float (-. (1.0 /. 0.0)) in
      x = pos_inf || x = neg_inf in
    (if F64.lt f F64.zero then "-" else "") ^
    if is_nan f then "NaN" else
    if is_inf x then "Infinity" else
      let x = abs x in (* non-neg, non-inf, non-nan case *)
      let f = F64.to_float (F64.of_bits x) in
      to_string f

  let i32_to_string i =
    let s = Printf.sprintf "%x" (abs (Int32.to_int (I32.to_bits i))) in
    let tobits_seq c = c |> tobits |> String.to_seq in
    let bits = s |> String.to_seq |> Seq.flat_map tobits_seq |> String.of_seq in
    (*Printf.printf "in hex: %s in bits: %s\n" s bits;*)
    let res = match String.index_opt bits '1', String.index_opt bits '0' with
      | Some onepos, Some zeropos when zeropos < onepos -> (* strip leading zeros before 1 *)
        String.sub bits onepos (String.length bits - onepos)
      | None, _ -> "0"
      | _, _ -> bits in
    if i < 0l then "-"^res else res

  let print_i32 och vs = match vs with
    | [Values.I32 i] -> Printf.fprintf och "%s\n" (i32_to_string i); flush och; []
    | [_] -> failwith (Printf.sprintf "print_i32 called wrong argument type")
    | _   -> failwith (Printf.sprintf "print_i32 called with %i arguments" (List.length vs))

  let print_f32 och vs = match vs with
    | [Values.F32 f] -> Printf.fprintf och "%s\n" (f32_to_string f); flush och; []
    | [_] -> failwith (Printf.sprintf "print_f32 called wrong argument type")
    | _   -> failwith (Printf.sprintf "print_f32 called with %i arguments" (List.length vs))

  let print_f64 och vs = match vs with
    | [Values.F64 f] -> Printf.fprintf och "%s\n" (f64_to_string f); flush och; []
    | [_] -> failwith (Printf.sprintf "print_f64 called wrong argument type")
    | _   -> failwith (Printf.sprintf "print_f64 called with %i arguments" (List.length vs))

  let print och vs = match vs with
    | [Values.I32 i] -> Printf.fprintf och "%s\n" (i32_to_string i); flush och; []
    | [Values.F32 f] -> Printf.fprintf och "%s\n" (f32_to_string f); flush och; []
    | [Values.F64 f] -> Printf.fprintf och "%s\n" (f64_to_string f); flush och; []
    | [_] -> failwith (Printf.sprintf "print called wrong argument type")
    | _   -> failwith (Printf.sprintf "print called with %i arguments" (List.length vs))

  let lookup och name t =
    match Utf8.encode name, t with
    | "log",   Types.(ExternFuncType (FuncType ([I32Type],[]))) ->
      Instance.ExternFunc (Func.alloc_host (Types.FuncType ([I32Type],[])) (print_i32 och))
    | "logfl", Types.(ExternFuncType (FuncType ([F32Type],[]))) ->
      Instance.ExternFunc (Func.alloc_host (Types.FuncType ([F32Type],[])) (print_f32 och))
    | "logfl", Types.(ExternFuncType (FuncType ([F64Type],[]))) ->
      Instance.ExternFunc (Func.alloc_host (Types.FuncType ([F64Type],[])) (print_f64 och))
    | _ -> raise Not_found

  let inst_run_mod och m =
    Import.register (Utf8.decode "imports") (lookup och);
    let protected_run f =
      try f () with
      | Eval.Trap (_,"integer divide by zero") ->
        Printf.fprintf och "RuntimeError integer division by zero\n"; flush_all ()

      | Eval.Trap (_,"unreachable executed") ->
        Printf.fprintf och "RuntimeError unreachable executed\n"; flush_all ()

      | Eval.Exhaustion (_,"call stack exhausted") ->
        Printf.fprintf och "stack overflow\n"; flush_all ()

      | Eval.Trap (_,"integer overflow")
      | Eval.Trap (_,"invalid conversion to integer") ->
        Printf.fprintf och "RuntimeError invalid conversion to integer or overflow\n"; flush_all ()
      | Eval.Trap (_,"out of bounds memory access") ->
        Printf.fprintf och "RuntimeError out of bounds memory access\n"; flush_all ()
  
      | Eval.Trap (_,"indirect call type mismatch") ->
        Printf.fprintf och "RuntimeError indirect call error\n"; flush_all ()
  
      | Eval.Trap (_,err) ->
        if String.length err > 21 && String.sub err 0 21 = "uninitialized element"
        then (Printf.fprintf och "RuntimeError indirect call error\n"; flush_all ())
        else Printf.fprintf och "Trap exception: %s\n" err; flush_all ()

      | Eval.Exhaustion (_,err) ->
        Printf.fprintf och "Exhaustion exception: %s\n" err; flush_all ()

      | Eval.Link (_,"data segment does not fit memory") ->
        Printf.fprintf och "LinkError data segment does not fit memory\n" ; flush_all () 

        (*    | _ (*Wasm.Error.Make.Error (_,err)*) ->
              Printf.fprintf och "other exception\n"; flush_all () (*true*)(*false*) *)in

    protected_run
      (fun () ->
         let inst = ref (Eval.init m (Import.link m)) in
         iter_exports
           (fun str f ->
              protected_run
                (fun () ->
                   let typ =
                     List.nth m.Source.it.types (Int32.to_int f.Source.it.Ast.ftype.Source.it) in
                   Printf.fprintf och "%s\n" str;
                   flush och;
                   ignore (print och (Eval.invoke (AstFunc (typ.Source.it, inst, f)) [])))) m)

  let run_module och m =
    try inst_run_mod och m; true with
    | Eval.Link (_,err) ->
      Printf.fprintf och "Link exception: %s\n" err; flush_all (); false
    | Eval.Crash (_,err) ->
      Printf.fprintf och "Crash exception: %s\n" err; flush_all (); false
  (* | Failure err ->
       Printf.fprintf och "Failure exception: %s\n" err; flush_all (); false *)
  (* | _ -> false *)
end

type result = Timeout | Res of bool

let timed_fork_prop sec p x cleanup =
  let a = Unix.fork() in match a with
  | 0  ->
    let _ = Unix.alarm sec in
    if p x
    then (ignore (Unix.alarm 0); exit 0) (*cancel alarm*)
    else (ignore (Unix.alarm 0); exit 2) (*cancel alarm*)
  (* | -1 -> print_endline "error accured on fork"; false *)
  | _  ->
    let childid, retcode = Unix.wait () in
    let _code, _s, res = (match retcode with
        | WEXITED code   -> code, "WEXITED", Res (0=code)
        | WSIGNALED code -> code, "WSIGNALED", if Sys.sigalrm=code then Timeout else Res false
        | WSTOPPED code  -> code, "WSTOPPED", Res false) in
    cleanup(); res

let run_int_test =
  Test.make ~name:"ref interpret internal run" ~count:10(*00*)
    ((*set_shrink Shrink.nil*) arb_module)
    (function m ->
       let och = open_out "tmp/tmp_spec" in
       match timed_fork_prop 10 (JSLikeEval.run_module och) m (fun () -> flush och; close_out och) with
       | Timeout -> false
       | Res res -> res)

let count = ref 0


let convert file_name eng =
  let cmd =
    Printf.sprintf
      "node ../javascript/convert.js %s %s > tmp/tmp_%s.js 2> tmp/error" file_name eng eng in
  Sys.command cmd

let run eng =
  let cmd =
    Printf.sprintf "timeout 10 %s tmp/tmp_%s.js > tmp/tmp_%s 2>&1" eng eng eng in
  Sys.command cmd

let timeout = 124
let okish code = code = 0 || code = timeout

let ch_tee_local_bug fname =
  0 = Sys.command ("grep -q 'tee_local' " ^ fname)

let run_diff_from_ocaml =
  Test.make ~name:"ref interpret vs. js-engines" ~count:100(*00*)
    (arb_module)
    (function m ->
       module_to_wasm m wasm_file_name;
       module_to_wat m wat_file_name;
       let v8res  = convert wasm_file_name "v8" in
       let smres  = convert wasm_file_name "sm" in
       let chres  = convert wasm_file_name "ch" in
       let jscres = convert wasm_file_name "jsc" in
       let res = 0 = v8res && 0 = smres && 0 = chres && 0 = jscres in
       let v8rc  = run "v8" in
       let smrc  = run "sm" in
       let chrc  = run "ch" in
       let jscrc = run "jsc" in
       let res = res && okish v8rc && v8rc = smrc && smrc = chrc && chrc = jscrc in
       let och = open_out "tmp/tmp_spec" in
       let spec_res =
         timed_fork_prop 10 (JSLikeEval.run_module och) m (fun () -> flush och; close_out och) in
       let res = res && (match spec_res with | Timeout -> v8rc = timeout | Res res -> res) in
       let res = res &&
          (if ch_tee_local_bug "tmp/tmp_ch"
           then true
           else 0 = Sys.command "cmp -s -n 5000 tmp/tmp_ch  tmp/tmp_jsc") &&
          0 = Sys.command "cmp -s -n 5000 tmp/tmp_jsc tmp/tmp_sm" &&
          0 = Sys.command "cmp -s -n 5000 tmp/tmp_sm  tmp/tmp_v8" &&
          0 = Sys.command "cmp -s -n 300  tmp/tmp_v8 tmp/tmp_spec" in
       if not res
       then
         begin
           ignore (Sys.command ("cp -f " ^ wat_file_name ^ " tmp/prev.wat"));
           ignore (Sys.command ("cp -f " ^ wasm_file_name ^ " tmp/prev.wasm"));
           ignore (Sys.command ("cp -f tmp/tmp_v8.js tmp/prev_v8.js"));
           ignore (Sys.command ("cp -f tmp/tmp_sm.js tmp/prev_sm.js"));
           ignore (Sys.command ("cp -f tmp/tmp_ch.js tmp/prev_ch.js"));
           ignore (Sys.command ("cp -f tmp/tmp_jsc.js tmp/prev_jsc.js"));
           ignore (Sys.command ("cp -f tmp/tmp_v8 tmp/prev_v8"));
           ignore (Sys.command ("cp -f tmp/tmp_sm tmp/prev_sm"));
           ignore (Sys.command ("cp -f tmp/tmp_ch tmp/prev_ch"));
           ignore (Sys.command ("cp -f tmp/tmp_jsc tmp/prev_jsc"));
           ignore (Sys.command ("cp -f tmp/tmp_spec tmp/prev_spec"));
	   ignore (Sys.command ("cp -f tmp/error tmp/prev_error"));
           incr count;
           module_to_wat m ("tmp/shrink" ^ (string_of_int !count) ^ ".wat")
         end;
       res)

let run_ext_test =
  Test.make ~name:"ref interpret external run" ~count:1(*00*)
    arb_module
    (function m ->
       module_to_wat m wat_file_name;
       Sys.command ("../spec/interpreter/wasm " ^ wat_file_name ^ " script.wast") = 0)

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
