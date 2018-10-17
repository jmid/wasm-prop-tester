#load "str.cma";;

open QCheck
open Printf
open Unix
open Str

type aexp =
  | Lit of int
  | Add of aexp * aexp
  | Sub of aexp * aexp
  | Mul of aexp * aexp
  | Div of aexp * aexp

let rec interpret ae = match ae with
  | Lit i -> i
  | Add (ae0, ae1) ->
    let v0 = interpret ae0 in
    let v1 = interpret ae1 in
    v0 + v1
  | Sub (ae0, ae1) ->
    let v0 = interpret ae0 in
    let v1 = interpret ae1 in
    v0 - v1
  | Mul (ae0, ae1) ->
    let v0 = interpret ae0 in
    let v1 = interpret ae1 in
    v0 * v1
  | Div (ae0, ae1) ->
    let v0 = interpret ae0 in
    let v1 = interpret ae1 in
    v0 / v1

let rec exp_to_string ae = match ae with
  | Lit i -> "Lit (" ^ string_of_int i ^ ")"
  | Add (ae0, ae1) ->
    let s0 = exp_to_string ae0 in
    let s1 = exp_to_string ae1 in
    "Add (" ^ s0 ^ "," ^ s1 ^ ")"
  | Sub (ae0, ae1) ->
    let s0 = exp_to_string ae0 in
    let s1 = exp_to_string ae1 in
    "Sub (" ^ s0 ^ "," ^ s1 ^ ")"
  | Mul (ae0, ae1) ->
    let s0 = exp_to_string ae0 in
    let s1 = exp_to_string ae1 in
    "Mul (" ^ s0 ^ "," ^ s1 ^ ")"
  | Div (ae0, ae1) ->
    let s0 = exp_to_string ae0 in
    let s1 = exp_to_string ae1 in
    "Div (" ^ s0 ^ "," ^ s1 ^ ")"

let leafgen = Gen.map (fun i -> Lit i) Gen.small_nat

let mygen =
  Gen.sized (Gen.fix (fun recgen n -> match n with
    | 0 -> leafgen
    | n ->
      Gen.oneof
	[leafgen;
	 Gen.map2 (fun l r -> Add(l,r)) (recgen(n/2)) (recgen(n/2));
	 Gen.map2 (fun l r -> Sub(l,r)) (recgen(n/2)) (recgen(n/2));
	 Gen.map2 (fun l r -> Mul(l,r)) (recgen(n/2)) (recgen(n/2));
	 (*Gen.map2 (fun l r -> Div(l,r)) (recgen(n/2)) (recgen(n/2));*) ]))


   let rec height ae = match ae with
     | Lit i -> 0
     | Add (ae0, ae1) ->
       let h0 = height ae0 in
       let h1 = height ae1 in
       1 + (max h0 h1)
     | Sub (ae0, ae1) ->
       let h0 = height ae0 in
       let h1 = height ae1 in
       1 + (max h0 h1)
     | Mul (ae0, ae1) ->
       let h0 = height ae0 in
       let h1 = height ae1 in
       1 + (max h0 h1)
     | Div (ae0, ae1) ->
       let h0 = height ae0 in
       let h1 = height ae1 in
       1 + (max h0 h1)

let arb_tree = make ~print:exp_to_string ~stats:[("tree height'", height)] mygen

(*To Wasm*)
let exp_to_wat ae =
  let rec exp_to_string ae = match ae with
    | Lit i -> "(i32.const " ^ string_of_int i ^ ")"
    | Add (ae0, ae1) ->
      let s0 = exp_to_string ae0 in
      let s1 = exp_to_string ae1 in
      "(i32.add " ^ s0 ^ " " ^ s1 ^ ")"
    | Sub (ae0, ae1) ->
      let s0 = exp_to_string ae0 in
      let s1 = exp_to_string ae1 in
      "(i32.sub " ^ s0 ^ " " ^ s1 ^ ")"
    | Mul (ae0, ae1) ->
      let s0 = exp_to_string ae0 in
      let s1 = exp_to_string ae1 in
      "(i32.mul " ^ s0 ^ " " ^ s1 ^ ")"
    | Div (ae0, ae1) ->
      let s0 = exp_to_string ae0 in
      let s1 = exp_to_string ae1 in
      "(i32.div_s " ^ s0 ^ " " ^ s1 ^ ")"
  in "(module (func (export \"aexp\") (result i32) " ^ exp_to_string ae ^ "))"

let file = "test_module2.wat"

let output_to_file ae = 
  let oc = open_out file in 
    fprintf oc "%s\n" (exp_to_wat ae);
    close_out oc

let print_tree ae = 
  print_endline (exp_to_wat ae)

let div_r = regexp ".*"
;;

(*
*)
let (<+>) = Iter.(<+>)
let rec tshrink e = match e with
  | Lit i -> Iter.map (fun i -> Lit i) (Shrink.int i)
  | Add (ae0, ae1) ->
     (Iter.of_list [ Lit 1; Lit (-1); ae0; ae1])
     <+> (Iter.map (fun ae0' -> Add (ae0',ae1)) (tshrink ae0))
     <+> (Iter.map (fun ae1' -> Add (ae0,ae1')) (tshrink ae1))
  | Sub (ae0, ae1) ->
     (Iter.of_list [ Lit 1; Lit (-1); ae0; ae1])
     <+> (Iter.map (fun ae0' -> Sub (ae0',ae1)) (tshrink ae0))
     <+> (Iter.map (fun ae1' -> Sub (ae0,ae1')) (tshrink ae1))
  | Mul (ae0, ae1) ->
     (Iter.of_list [Lit 1; Lit (-1); ae0; ae1])
     <+> (Iter.map (fun ae0' -> Mul (ae0',ae1)) (tshrink ae0))
     <+> (Iter.map (fun ae1' -> Mul (ae0,ae1')) (tshrink ae1))
  | Div (ae0, ae1) ->
     (Iter.of_list [Lit 1; Lit (-1); ae0; ae1])
     <+> (Iter.map (fun ae0' -> Div (ae0',ae1)) (tshrink ae0))
     <+> (Iter.map (fun ae1' -> Div (ae0,ae1')) (tshrink ae1))
;;

let arithmetic =
  Test.make ~count:1000 
  (*arb_tree*)
  (*(set_shrink tshrink arb_tree)*)
  (set_shrink tshrink arb_tree)
  (fun e -> 
    output_to_file e; 
    (*Sys.command ("wasm test_module2.wat -e '(invoke \"aexp\")'");*)
    (*print_endline (string_of_int (interpret e));*)
    (*let ic = open_process_in ("wasm " ^ file ^ " -e '(invoke \"aexp\")'") in
      try
        let line = input_line ic in
          let r = regexp (string_of_int (interpret e)) in 
            close_process_in ic;
            string_match r line 0
      with End_of_file ->
        close_process_in ic;
        true*)
      Sys.command ("wat2wasm test_module2.wat -o test_module2.wasm");
      Sys.command ("node ../javascript/convert.js test_module2.wasm > test_module2.js");
      (*Sys.command ("eshost -h Cha*,Sp*,Ja*,V8* -u test_module2.js") = 0;*)
      let ch = Sys.command ("ch test_module2.js") 
      and v8 = Sys.command ("v8 test_module2.js")
      and sm = Sys.command ("sm test_module2.js") in
          ch = v8 && v8 = sm
      (*let ic = open_process_in ("eshost -h Cha*,Sp*,Ja*,V8* -u test_module2.js") in
        try
          let line = input_line ic in
            (*print_endline line;*)
            close_process_in ic
        with End_of_file ->
          close_process_in ic
      *)
    )
;;

(*
QCheck_runner.set_seed(23288955);;
QCheck_runner.run_tests ~verbose:true [ arithmetic; ] ;;
*)