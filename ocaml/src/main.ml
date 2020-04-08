open QCheck
open Generator

let count = ref 0

let compare_engines_test =
  Test.make ~name:"compare engines" ~count:100 (*10000*)
    arb_module
    (function m ->
       module_to_wasm m wasm_file_name;
       let res = Sys.command ("../script/compare_engines.sh " ^ wasm_file_name) = 0 in
       if res
       then res
       else (incr count; module_to_wat m ("tmp/shrink" ^ (string_of_int !count) ^ ".wat"); res)
       (*else (module_to_wat m wat_file_name; res)*))
;;

QCheck_runner.run_tests_main
  [
    (*compare_engines_test;*)
    (*run_int_test;*)
    run_diff_from_ocaml;
    (*    run_ext_test;*)
    (*implementation_test;*) (*implementation_stat_test;*) (*conversion_test; wabt_test;*) ] ;;
