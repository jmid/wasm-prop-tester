open QCheck
open Generator

let compare_engines_test =
  Test.make ~name:"compare engines" ~count:100 (*10000*)
  arb_module
  (function m ->
    module_to_wat m wat_file_name;
    Sys.command ("../script/compare_engines.sh " ^ wat_file_name) = 0)
;;

QCheck_runner.run_tests_main
  [
    compare_engines_test;
    (* run_test; *)
    (*implementation_test;*) (*implementation_stat_test;*) (*conversion_test; wabt_test;*) ] ;;
