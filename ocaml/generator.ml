#load "wasm.cmo";;

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
*)

let empty = Wasm.Ast.empty_module;;
let empty_module = {Wasm.Source.at = Wasm.Source.no_region; Wasm.Source.it = empty};;
let arrange_m = Wasm.Arrange.module_ empty_module;;
Wasm.Sexpr.print 0 arrange_m;;