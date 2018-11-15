

open Wasm

module Helper =
struct    
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

end