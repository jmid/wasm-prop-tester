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

let arb_tree = make ~print:exp_to_string ~stats:[("Tree height'", height)] mygen

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
  Test.make ~name:"Arithmetic expressions" ~count:1000 
  (set_shrink tshrink arb_tree)
  (fun e -> 
    output_to_file e; 
    Sys.command ("../script/compare.sh test_module2.wat") = 0
  )
;;

(*
QCheck_runner.set_seed(23288955);;
QCheck_runner.run_tests ~verbose:true [ arithmetic; ] ;;
*)