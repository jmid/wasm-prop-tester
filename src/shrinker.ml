open Wasm
open QCheck
open Helper

let i32_shrink x yield = 
    let y = ref x in
    (* try some divisors *)
    while !y < -2l || !y > 2l do y := Int32.div !y 2l; yield (Int32.sub x !y); done; (* fast path *)
    if x>0l then yield (Int32.pred x);
    if x<0l then yield (Int32.succ x);
    ()

let i64_shrink x yield = 
    let y = ref x in
    (* try some divisors *)
    while !y < -2L || !y > 2L do y := Int64.div !y 2L; yield (Int64.sub x !y); done; (* fast path *)
    if x>0L then yield (Int64.pred x);
    if x<0L then yield (Int64.succ x);
    ()

let i64_shrink i =
  if i = I64.zero then Iter.empty else Iter.return I64.zero

let f32_shrink i =
  if i = F32.zero then Iter.empty else Iter.return F32.zero

let f64_shrink i =
  if i = F64.zero then Iter.empty else Iter.return F64.zero
(*
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
*)

let rec max_local il = match il with
  | [] -> -1l
  | (e: Ast.instr)::rst ->
    match e.it with
    | Ast.Block (_,l) -> max (max_local rst) (max_local l)
    | Ast.Loop (_,l)  -> max (max_local rst) (max_local l)
    | Ast.If (_,l,l') -> max (max_local rst) (max (max_local l) (max_local l'))
    | Ast.LocalGet v  -> max v.it (max_local rst)
    | Ast.LocalSet v  -> max v.it (max_local rst)
    | Ast.LocalTee v  -> max v.it (max_local rst)
    | _               -> max_local rst

let rec rename_fun trn frn il = match il with
  | [] -> il
  | (e: Ast.instr)::rst ->
    match e.it with
    | Ast.Call i       ->
      as_phrase (Ast.Call (as_phrase (frn i.Source.it)))::(rename_fun trn frn rst)
    | Ast.CallIndirect i ->
      as_phrase (Ast.CallIndirect (as_phrase (trn i.Source.it)))::(rename_fun trn frn rst)
    | Ast.Block (st,l) ->
      as_phrase (Ast.Block (st,rename_fun trn frn l))::(rename_fun trn frn rst)
    | Ast.Loop (st,l)  ->
      as_phrase (Ast.Loop (st,rename_fun trn frn l))::(rename_fun trn frn rst)
    | Ast.If (st,l,l') ->
      as_phrase (Ast.If (st, rename_fun trn frn l, rename_fun trn frn l'))::(rename_fun trn frn rst)
    | _                -> e::rename_fun trn frn rst

let renamed_index oldfs newfs i =
  let f = List.nth oldfs i in
  let rec loop j fs = match fs with
    | [] -> None
    | f'::fs -> if f=f' then Some j else loop (j+1) fs in
  loop 0 newfs

let rec contains_label is = match is with
  | [] -> false
  | i::is ->
    (match i.Source.it with
     | Ast.Unreachable
     | Ast.Nop
     | Ast.Drop
     | Ast.Select
     | Ast.Return
     | Ast.Call _
     | Ast.CallIndirect _
     | Ast.LocalGet _
     | Ast.LocalSet _
     | Ast.LocalTee _
     | Ast.GlobalGet _
     | Ast.GlobalSet _
     | Ast.Load _
     | Ast.Store _
     | Ast.MemorySize
     | Ast.MemoryGrow
     | Ast.Const _
     | Ast.Test _
     | Ast.Compare _
     | Ast.Unary _
     | Ast.Binary _
     | Ast.Convert _ -> false
     | Ast.Block (_,js) -> contains_label js
     | Ast.Loop (_,js) -> contains_label js
     | Ast.If (_,is1,is2) -> contains_label is1 || contains_label is2
     | Ast.Br _ -> true
     | Ast.BrIf _ -> true
     | Ast.BrTable (_,_) -> true
     ) || contains_label is

let const_zero_instr t =
  let zero = match t with
    | Types.I32Type -> Values.I32 I32.zero
    | Types.I64Type -> Values.I64 I64.zero
    | Types.F32Type -> Values.F32 F32.zero
    | Types.F64Type -> Values.F64 F64.zero in
  as_phrase (Ast.Const (as_phrase zero))

(*  instr_list_shrink : module_' -> value_type list -> instr list -> instr list Iter.t *)
let rec instr_list_shrink m' ls is = match is with
  | [] -> Iter.empty
  | i::is ->
    Iter.(
      (match is with
       | [] -> Iter.empty
       | j::is ->
         (match is with
          | [] -> empty
          | k::is ->
            (match i.Source.it,j.Source.it,k.Source.it with
             | Ast.Const _    , Ast.Const _    , Ast.Select
             | Ast.Const _    , Ast.LocalGet _ , Ast.Select
             | Ast.Const _    , Ast.GlobalGet _, Ast.Select
             | Ast.LocalGet _ , Ast.Const _    , Ast.Select
             | Ast.LocalGet _ , Ast.LocalGet _ , Ast.Select
             | Ast.LocalGet _ , Ast.GlobalGet _, Ast.Select
             | Ast.GlobalGet _, Ast.Const _    , Ast.Select
             | Ast.GlobalGet _, Ast.LocalGet _ , Ast.Select
             | Ast.GlobalGet _, Ast.GlobalGet _, Ast.Select -> return is
             | Ast.Const _    , Ast.Const _    , Ast.Compare _
             | Ast.Const _    , Ast.LocalGet _ , Ast.Compare _
             | Ast.Const _    , Ast.GlobalGet _, Ast.Compare _
             | Ast.LocalGet _ , Ast.Const _    , Ast.Compare _
             | Ast.LocalGet _ , Ast.LocalGet _ , Ast.Compare _
             | Ast.LocalGet _ , Ast.GlobalGet _, Ast.Compare _
             | Ast.GlobalGet _, Ast.Const _    , Ast.Compare _
             | Ast.GlobalGet _, Ast.LocalGet _ , Ast.Compare _
             | Ast.GlobalGet _, Ast.GlobalGet _, Ast.Compare _ ->
               return ((as_phrase (Ast.Const (as_phrase (Values.I32 I32.zero))))::is)
             | _, _, _ -> empty))
         <+>
         (match i.Source.it,j.Source.it with
           | Ast.Const _    , Ast.BrIf _
           | Ast.LocalGet _ , Ast.BrIf _
           | Ast.GlobalGet _, Ast.BrIf _
           | Ast.Const _    , Ast.Drop
           | Ast.LocalGet _ , Ast.Drop
           | Ast.GlobalGet _, Ast.Drop
           (*   | Ast.Block _    , Ast.Drop  *)    (* only when Block returns something *)
           | Ast.Const _    , Ast.Binary _
           | Ast.LocalGet _ , Ast.Binary _
           | Ast.GlobalGet _, Ast.Binary _
           | Ast.Const _, Ast.LocalSet _
           | Ast.Const _, Ast.GlobalSet _
           | Ast.GlobalGet _, Ast.GlobalSet _
           | Ast.LocalGet _ , Ast.LocalSet _
           | Ast.GlobalGet _, Ast.LocalSet _
           | Ast.LocalGet _ , Ast.GlobalSet _ -> return is
           | Ast.Const _    , Ast.Test _
           | Ast.LocalGet _ , Ast.Test _
           | Ast.GlobalGet _, Ast.Test _ ->
             return ((as_phrase (Ast.Const (as_phrase (Values.I32 I32.zero))))::is)
           | Ast.Const _    , Ast.If (_,is1,is2)
           | Ast.LocalGet _ , Ast.If (_,is1,is2)
           | Ast.GlobalGet _, Ast.If (_,is1,is2) ->
             (if contains_label is1 then empty else return (is1@is))
             <+>
             (if contains_label is2 then empty else return (is2@is))
           | Ast.Const _, Ast.BrTable ([],l)  -> return (as_phrase (Ast.Br l)::is)
           | Ast.Unreachable, Ast.Unreachable -> return (j::is) (* bubble unreach down *)
           | Ast.Unreachable,               _ -> return (j::i::is)
           | _, _ -> empty))
      <+>
      (match i.Source.it with
       | Ast.Nop
       | Ast.MemoryGrow
       | Ast.Unary _    -> return is         (* no change in stack -> omit *)
       | Ast.MemorySize -> return ((const_zero_instr Types.I32Type)::is)
       | Ast.Convert cvt ->
         let res_type = match cvt with
           | Values.I32 _ -> Types.I32Type
           | Values.I64 _ -> Types.I64Type
           | Values.F32 _ -> Types.F32Type
           | Values.F64 _ -> Types.F64Type in
         return ((as_phrase Ast.Drop)::(const_zero_instr res_type)::is)
       | Ast.GlobalSet g ->                  (* change GlobalSets into Drop *)
         return ((as_phrase Ast.Drop)::is)
         <+> (* or replace GlobalSet with another, lower-indexed one *)
         (let gs = m'.Ast.globals in
          let glob = List.nth gs (Int32.to_int g.it) in
          let gtype = glob.Source.it.Ast.gtype in
          let i = find_index (fun g -> gtype = g.Source.it.Ast.gtype) gs in
          if i < (Int32.to_int g.it)
          then return (as_phrase (Ast.GlobalSet (as_phrase (I32.of_int_s i)))::is)
          else empty)
       | Ast.GlobalGet g ->                  (* change GlobalGets into Consts *)
         let gs = m'.Ast.globals in
         let glob = List.nth gs (Int32.to_int g.it) in
         let GlobalType (vtype,_) = glob.Source.it.Ast.gtype in
         return ((const_zero_instr vtype)::is)
         <+> (* or replace GlobalGet with another, lower-indexed one *)
         (let i = find_index
                    (fun g -> let GlobalType (vtype',_) = g.Source.it.Ast.gtype in
                              vtype = vtype') gs in
          if i < (Int32.to_int g.it)
          then return (as_phrase (Ast.GlobalGet (as_phrase (I32.of_int_s i)))::is)
          else empty)

       | Ast.LocalTee l ->
         return is         (* no change in stack -> omit *)
         <+> (* or replace LocalTee with another, lower-indexed one *)
         let _,i = first_type_index l ls in
         if i < l.Source.it
         then return (as_phrase (Ast.LocalTee (as_phrase i))::is)
         else empty
       | Ast.LocalSet l ->
         return ((as_phrase Ast.Drop)::is)
         <+> (* or replace LocalSet with another, lower-indexed one *)
         let _,i = first_type_index l ls in
         if i < l.Source.it
         then return (as_phrase (Ast.LocalSet (as_phrase i))::is)
         else empty
       | Ast.LocalGet l ->
         let my_type,i = first_type_index l ls in
         return (const_zero_instr my_type::is)
         <+> (* or replace LocalGet with another, lower-indexed one *)
         if i < l.Source.it
         then return (as_phrase (Ast.LocalGet (as_phrase i))::is)
         else empty

       | Ast.Load l ->
         return ((as_phrase Ast.Drop)::(const_zero_instr l.Ast.ty)::is)
         <+>
         map (fun align' -> as_phrase (Ast.Load { l with align = align' })::is)
           (Shrink.int l.align)
         <+>
         map (fun offset' -> as_phrase (Ast.Load { l with offset = offset' })::is)
           (i32_shrink l.offset)
       | Ast.Call n ->
         let fs = m'.Ast.funcs in
         let n = Int32.to_int n.Source.it in
         if n < List.length m'.imports
         then return ((as_phrase Ast.Drop)::is)  (* replace import call with drop *)
         else
           let f = List.nth fs (n - List.length m'.imports) in
           let ftype = List.nth m'.Ast.types (Int32.to_int f.it.ftype.it) in
           let FuncType (input,output) = ftype.it in
           let rec build_drops n = match n with
             | 0 -> []
             | _ -> (as_phrase Ast.Drop)::build_drops (n-1) in
           (match output with
            | []  -> return (build_drops (List.length input) @ is)
            | [t] -> return (build_drops (List.length input) @ [const_zero_instr t] @ is)
            | _   -> empty)

       | Ast.CallIndirect n ->
         let ftype = List.nth m'.Ast.types (Int32.to_int n.it) in
         let FuncType (input,output) = ftype.it in
         let rec build_drops n = match n with
           | 0 -> []
           | _ -> (as_phrase Ast.Drop)::build_drops (n-1) in
         (match output with
          | []  -> return (build_drops (1 + List.length input) @ is)
          | [t] -> return (build_drops (1 + List.length input) @ [const_zero_instr t] @ is)
          | _   -> empty)

       | Ast.Return ->
         if is=[] then empty else return [i] (* delete instrs after return *)
       | Ast.Block (sts,[])  -> return is    (* remove empty block *)
       | Ast.Block (sts,is') ->
         map (fun is'' -> as_phrase (Ast.Block (sts,is''))::is) (instr_list_shrink m' ls is')
       | Ast.Loop (sts,is') ->
         (match sts with
          | ValBlockType None     -> return is
          | ValBlockType (Some t) -> return (const_zero_instr t::is)
          | VarBlockType _        -> empty)
         <+>
         (if contains_label is' then empty else return (is'@is))
         <+>
         map (fun is'' -> as_phrase (Ast.Loop (sts,is''))::is) (instr_list_shrink m' ls is')
       | Ast.If (sts,is1,is2) ->
         (match sts with
          | ValBlockType (Some t) ->
            of_list
              [ (as_phrase Ast.Drop)::(const_zero_instr t)::is;
                as_phrase (Ast.If (sts,[const_zero_instr t],is2))::is;
                as_phrase (Ast.If (sts,is1,[const_zero_instr t]))::is ]
          | ValBlockType None
          | VarBlockType _        -> empty)
         <+>
         (if contains_label is1 then empty else return ((as_phrase Ast.Drop)::is1@is))
         <+>
         (if contains_label is2 then empty else return ((as_phrase Ast.Drop)::is2@is))
         <+>
         (map (fun is' -> as_phrase (Ast.If (sts,is',is2))::is) (instr_list_shrink m' ls is1))
         <+>
         (map (fun is' -> as_phrase (Ast.If (sts,is1,is'))::is) (instr_list_shrink m' ls is2))
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
      map (fun is' -> i::is') (instr_list_shrink m' ls is)
    )

let global_shrink m' g =
  let g' = g.Source.it in
  Iter.map
    (fun is -> as_phrase { g' with Ast.value = as_phrase is })
    (instr_list_shrink m' [] g'.Ast.value.Source.it) (*no locals*)
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

let rec shrink_functions m' (fs : Ast.func list) (types : Wasm.Ast.type_ list) =
  match fs with
  | [] -> Iter.empty
  | f::fs ->
    let fun_typ = List.nth types (Int32.to_int f.it.ftype.it) in
    Iter.(
      (match f.it.Ast.body with
         | []  -> empty (* don't shrink empty body *)
         | [i] -> empty (* or one instr body *)
         | _  ->
           map
             (fun body' -> (as_phrase { f.it with Ast.body = body' })::fs)
             (match fun_typ.Source.it with
              | Types.FuncType ([],[])  -> return []
              | Types.FuncType (_,[rt]) -> return [const_zero_instr rt]
              | _ -> empty))
      <+>
      (let Types.FuncType (params,_) = fun_typ.Source.it in
       let locals = f.Source.it.Ast.locals in
       let locals_len = List.length locals in
       let params_locals = params @ locals in
       let params_len = List.length params in
       (map
          (fun body' -> (as_phrase { f.it with Ast.body = body' })::fs)
          (instr_list_shrink m' params_locals f.Source.it.Ast.body))
       <+>
       if locals = []
       then empty
       else
         let max_local = Int32.to_int (max_local f.Source.it.Ast.body) in
         if max_local < params_len + locals_len - 1
         then
           let locals' =
             if max_local < params_len
             then []
             else take (1 + max_local - params_len) locals in
           return ((as_phrase { f.it with Ast.locals = locals'})::fs)
         else empty)
      <+>
      map (fun fs' -> f::fs') (shrink_functions m' fs types))

(* attempt at a better string shrinker *)
let shrink_string s yield =
  let n = String.length s in
    let chunk_size = ref n in
    while !chunk_size > 0 do
      for i=0 to n - !chunk_size do
        (* remove elements in [i .. i+!chunk_size] *)
        let s' = Bytes.init (n - !chunk_size)
          (fun j -> if j<i then s.[j] else s.[j + !chunk_size])
        in
        yield (Bytes.unsafe_to_string s')
      done;
      chunk_size := !chunk_size / 2;
    done

let shrink_string s = match s with
  | "" -> Iter.empty
  | _  ->
    let len = String.length s in
(*  if len / 4 > 0
    then
      Iter.of_list [
        String.sub s 0 (len - (len / 4));
        String.sub s 0 (len - 1);
      ]
    else *)
      Iter.return (String.sub s 0 (len - 1))

let shrink_data m' s =
  Iter.(
    (map (fun c' -> as_phrase {s.Source.it with Ast.offset = as_phrase c'})
       (instr_list_shrink m' [] s.Source.it.Ast.offset.Source.it)) (*no locals*)
    <+>
    (map (fun s' -> as_phrase {s.Source.it with Ast.init = s'})
       (shrink_string s.Source.it.Ast.init)))

(*let shrink_limits (l : Int32.t Wasm.Types.limits) = Shrink.nil l*)

let shrink_memory (m : Wasm.Ast.memory) =
  let MemoryType l = m.it.Wasm.Ast.mtype in
  Iter.map
    (fun max_opt -> as_phrase { Wasm.Ast.mtype = MemoryType { l with max = max_opt }})
    (Shrink.option i32_shrink l.Wasm.Types.max)

let adjust_exports new_fun_idx exports = 
  List.map (fun e ->
      let e' = e.Source.it in
      match e'.Ast.edesc.it with
       | FuncExport v -> as_phrase { e' with edesc =
                           as_phrase (Ast.FuncExport (as_phrase (new_fun_idx v.it))) }
       | _ -> e) exports

let adjust_imports new_fun_idx imports = 
  List.map (fun e ->
      let e' = e.Source.it in
      match e'.Ast.idesc.it with
       | FuncImport v -> as_phrase { e' with idesc =
                           as_phrase (Ast.FuncImport (as_phrase (new_fun_idx v.it))) }
       | _ -> e) imports

let adjust_funcs new_type_idx new_fun_idx funcs =
  List.map (fun f ->
      let ftype' = as_phrase (new_type_idx f.Source.it.Ast.ftype.it) in
      let body' = rename_fun new_type_idx new_fun_idx f.Source.it.Ast.body in
      as_phrase { f.Source.it with ftype = ftype';
                                   body = body' }) funcs

let module_valid m = try Valid.check_module m; true with Valid.Invalid (_,_) -> false

exception TypeRemoved of int32
exception FuncRemoved of int32

let module_shrink (m : Wasm.Ast.module_' Wasm.Source.phrase) =
  Iter.((* shrink funcs and types combined *)
        (try (* 3 imports, start, 3 exports *)
           let num_imps = List.length m.it.Ast.imports in
           let num_start = if m.it.Ast.start = None then 0 else 1 in
           let fixed_ts,rest_ts = split (num_imps + num_start) (*7*) m.it.Ast.types in
           let fixed_fs,rest_fs = split num_start (*4*) m.it.Ast.funcs in
           let rest = List.combine rest_ts rest_fs in
           filter module_valid
             (Shrink.list rest >>= fun rest' ->
              let rest_ts,rest_fs = List.split rest' in
              let types' = fixed_ts@rest_ts in
              let funcs' = fixed_fs@rest_fs in
              let new_fun_idx i =
                if Int32.to_int i < num_imps
                then i (* imports untouched *)
                else 
                  match renamed_index m.it.Ast.funcs funcs' (Int32.to_int i - num_imps) with
                  | None -> raise (FuncRemoved i)
                  | Some j -> Int32.of_int (j + num_imps) in (* add imports again *)
              let new_type_idx i =
                if Int32.to_int i < num_imps
                then i (* imports untouched *)
                else 
                  match renamed_index m.it.Ast.types types' (Int32.to_int i) with
                  | None -> raise (TypeRemoved i)
                  | Some j -> Int32.of_int j in
              try
                let funcs' = adjust_funcs new_type_idx new_fun_idx funcs' in
                let exps'  = adjust_exports new_fun_idx m.it.Ast.exports in
                return
                  (as_phrase { m.it with Ast.types = types';
                                         Ast.funcs = funcs';
                                         Ast.exports = exps';
                                         (* import types + imports untouched *)
                             })
              with
                (FuncRemoved _)
              | (TypeRemoved _) -> empty)
         with Invalid_argument _ -> empty)
        <+> (* shrink imports and types combined *)
        (try (* 3 imports, start, 3 exports *)
           let imports = m.it.Ast.imports in
           let num_imps = List.length imports in
           let imp_ts,rest_ts = split num_imps m.it.Ast.types in
           let ts_and_imps = List.combine imp_ts imports in
           filter module_valid
             (Shrink.list ts_and_imps >>= fun ts_and_imps' ->
              let imp_ts',imp_fs = List.split ts_and_imps' in
              let num_imps' = List.length imp_fs in
              let types' = imp_ts'@rest_ts in
              let new_fun_idx i = (* adj. for #imports *)
                if Int32.to_int i < List.length imports
                then match renamed_index imports imp_fs (Int32.to_int i) with
                  | None -> raise (FuncRemoved i)
                  | Some j -> Int32.of_int j
                else Int32.(of_int (to_int i - num_imps + num_imps')) in
              let new_type_idx i = (* adj. for #imports *)
                if Int32.to_int i < List.length imports
                then match renamed_index imp_ts imp_ts' (Int32.to_int i) with
                  | None -> raise (TypeRemoved i)
                  | Some j -> Int32.of_int j
                else Int32.(of_int (to_int i - num_imps + num_imps')) in
              try
                let funcs' = adjust_funcs new_type_idx new_fun_idx m.it.Ast.funcs in
                let exps'  = adjust_exports new_fun_idx m.it.Ast.exports in
                let imp_fs' = adjust_imports new_type_idx imp_fs in
                return
                  (as_phrase { m.it with Ast.types = types';
                                         Ast.funcs = funcs';
                                         Ast.exports = exps';
                                         Ast.imports = imp_fs'; })
              with
                (FuncRemoved _)
              | (TypeRemoved _) -> empty)
         with Invalid_argument _ -> empty)
        <+>
        map (* shrink func bodies *)
          (fun funs' -> as_phrase { m.it with Ast.funcs = funs' })
          (shrink_functions m.it m.it.Ast.funcs m.it.Ast.types)
        <+> (* remove unneeded funcs *)
        (let fixed,rest = split 4 m.it.Ast.funcs in (* start +3 exports, 3 imports not counted *)
         filter module_valid
           (map
              (fun rest' -> as_phrase { m.it with Ast.funcs = fixed@rest' })
              (Shrink.list rest)))
        <+> (* remove start *)
        (match m.it.Ast.start with
         | None   -> empty
         | Some _ -> return (as_phrase { m.it with Ast.start = None }))
        <+> (* reduce exports *)
        filter module_valid
          (map (fun exps -> as_phrase { m.it with Ast.exports = exps })
          (Shrink.list m.it.Ast.exports))
        <+> (* reduce globals *)
        filter module_valid
          (map (fun gs -> as_phrase { m.it with Ast.globals = gs })
             (Shrink.list ~shrink:(global_shrink m.it) m.it.Ast.globals))
        <+> (* reduce declared types *)
        filter module_valid
          (map (fun ts -> as_phrase { m.it with Ast.types = ts }) (Shrink.list m.it.Ast.types))
        <+> (* remove singleton table *)
        filter module_valid
          (map (fun tbs -> as_phrase { m.it with Ast.tables = tbs }) (Shrink.list m.it.Ast.tables))
        <+> (* shrink elems *)
        filter module_valid
          (map (fun es -> as_phrase { m.it with Ast.elems = es }) (Shrink.list m.it.Ast.elems))
        <+> (* remove singleton memory *)
        filter module_valid
          (map (fun ms -> as_phrase { m.it with Ast.memories = ms }) (Shrink.list ~shrink:shrink_memory m.it.Ast.memories))
        <+> (* shrink data segment *)
        map (fun ds -> as_phrase { m.it with Ast.data = ds })
          (Shrink.list ~shrink:(shrink_data m.it) m.it.Ast.data)
      )
