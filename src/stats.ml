open QCheck
open Wasm
open Helper
open Generator

let stat_dir_name = "stat"

let timestamp = string_of_float (Unix.time())

let stat_dir = match Sys.file_exists stat_dir_name with
  | true  -> ()
  | false -> Unix.mkdir stat_dir_name 0o775

let stat_file_name = stat_dir_name ^ "/" ^ timestamp ^ "stat." ^ "csv"

let funcs_number (m: Ast.module_) = List.length m.it.funcs

let call_func_length (m: Ast.module_) = 
  let f = List.nth m.it.funcs 1 in
  instrs_length f.it.body

let funcs_length (m: Ast.module_) = 
  List.fold_left (fun s (f: Ast.func) -> s + instrs_length f.it.body) 0 m.it.funcs

let elems_length (m: Ast.module_) = List.length m.it.elems

let globals_length (m: Ast.module_) = List.length m.it.globals

let data_length (m: Ast.module_) = List.length m.it.data

let rec count_instrs p il = match il with
  | [] -> 0
  | (e: Ast.instr)::rst ->
    let count = if p e.it then 1 else 0 in
    count +
    match e.it with
    | Ast.Block (_,l) -> count_instrs p rst + count_instrs p l
    | Ast.Loop (_,l)  -> count_instrs p rst + count_instrs p l
    | Ast.If (_,l,l') -> count_instrs p rst + count_instrs p l + count_instrs p l'
    | _               -> count_instrs p rst

let instr_number p (m: Ast.module_) =
  List.fold_left (fun s (f: Ast.func) -> s + count_instrs p f.it.body) 0 m.it.funcs

let unreach_number   = instr_number ((=) Ast.Unreachable)
let nop_number       = instr_number ((=) Ast.Nop)
let drop_number      = instr_number ((=) Ast.Drop)
let select_number    = instr_number ((=) Ast.Select)
let block_number     = instr_number (function Ast.Block _ -> true | _ -> false)
let loop_number      = instr_number (function Ast.Loop _ -> true | _ -> false)
let if_number        = instr_number (function Ast.If _ -> true | _ -> false)
let br_number        = instr_number (function Ast.Br _ -> true | _ -> false)
let brif_number      = instr_number (function Ast.BrIf _ -> true | _ -> false)
let brtable_number   = instr_number (function Ast.BrTable _ -> true | _ -> false)
let return_number    = instr_number ((=) Ast.Return)
let call_number      = instr_number (function Ast.Call _ -> true | _ -> false)
let callindir_number = instr_number (function Ast.CallIndirect _ -> true | _ -> false)
let localget_number  = instr_number (function Ast.LocalGet _ -> true | _ -> false)
let localset_number  = instr_number (function Ast.LocalSet _ -> true | _ -> false)
let localtee_number  = instr_number (function Ast.LocalTee _ -> true | _ -> false)
let globalget_number = instr_number (function Ast.GlobalGet _ -> true | _ -> false)
let globalset_number = instr_number (function Ast.GlobalSet _ -> true | _ -> false)
let load_number      = instr_number (function Ast.Load _ -> true | _ -> false)
let store_number     = instr_number (function Ast.Store _ -> true | _ -> false)
let memsize_number   = instr_number ((=) Ast.MemorySize)
let memgrow_number   = instr_number ((=) Ast.MemoryGrow)
let const_number     = instr_number (function Ast.Const _ -> true | _ -> false)
let test_number      = instr_number (function Ast.Test _ -> true | _ -> false)
let compare_number   = instr_number (function Ast.Compare _ -> true | _ -> false)
let unary_number     = instr_number (function Ast.Unary _ -> true | _ -> false)
let binary_number    = instr_number (function Ast.Binary _ -> true | _ -> false)
let convert_number   = instr_number (function Ast.Convert _ -> true | _ -> false)

let print_number = instr_number (function Ast.Call v -> List.mem v.it [0l;1l;2l] | _ -> false)

let stat_to_file m = 
  let oc = open_out_gen [Open_append; Open_creat] 0o666 stat_file_name in
    Printf.fprintf oc "%d;%d;%d;%d;%d;%d;%d;%d\n" (funcs_number m) (call_func_length m) (funcs_length m) (elems_length m) (globals_length m) (data_length m) (call_number m) (callindir_number m);
    close_out oc

let offline_stat_test =
  Test.make ~name:"Offline generator stats" ~count:1000
  arb_module
  (function m ->
    stat_to_file m;
    module_to_wat m wat_file_name;
    Sys.command ("script/compare_engines.sh " ^ wat_file_name ^ " " ^ timestamp) = 0)

let percentage f m = 100 * f m / funcs_length m

let stat_test =
  Test.make ~name:"Instruction stats" ~count:1000
    (set_stats
       [("number of funs",   funcs_number);
        ("total fun length", funcs_length);
        ("elems length",     elems_length);
        ("globals length",   globals_length);
        ("data length",      data_length);

        (* there are 28 different instrs *)

        (* we can count them directly: *)
        (*("number of calls",  call_number);*)
        (*("number of indir calls",  callindir_number);*)

        (* or calculate stats in percent: *)
        ("unreachable %",  percentage unreach_number);
        ("nop %",          percentage nop_number);
        ("drop %",         percentage drop_number);
        ("select %",       percentage select_number);
        ("block %",        percentage block_number);
        ("loop %",         percentage loop_number);
        ("if %",           percentage if_number);
        ("br %",           percentage br_number);
        ("brif %",         percentage brif_number);
        ("brtable %",      percentage brtable_number);
        ("return %",       percentage return_number);
        ("call %",         percentage call_number);
        ("callindir %",    percentage callindir_number);
        ("localget %",     percentage localget_number);
        ("localset %",     percentage localset_number);
        ("localtee %",     percentage localtee_number);
        ("globalget %",    percentage globalget_number);
        ("globalset %",    percentage globalset_number);
        ("load %",         percentage load_number);
        ("store %",        percentage store_number);
        ("memorysize %",   percentage memsize_number);
        ("memorygrow %",   percentage memgrow_number);
        ("const %",        percentage const_number);
        ("test %",         percentage test_number);
        ("compare %",      percentage compare_number);
        ("unary %",        percentage unary_number);
        ("binary %",       percentage binary_number);
        ("convert %",      percentage convert_number);

        ("print num",      print_number);
       ]
       arb_module)
    (fun m -> true)

;;
QCheck_runner.run_tests_main
  [(*offline_stat_test;*)
   stat_test]
