open Wasm
open QCheck
open Generator

let wat_shrunk_name = tmp_dir_name ^ "/" ^ "shrunk_module.wat";;

exception Done
let iter_helper n m =
  try
    let count = ref n in
    module_shrink m (fun m' ->
        if 0 = !count
        then raise Done
        else
          try
            (Valid.check_module m'; decr count)
          with e ->
            module_to_wat m' wat_shrunk_name;
            raise e);
    true
  with Done -> true

let shrink_test =
  Test.make ~name:"shrink test" ~count:1000 (*10000*)
    (pair
       (arb_module)
       QCheck.small_nat
       (*(make ~print:Print.int ~shrink:Shrink.int (Gen.return 10))*)
    )
  (function (m,n) ->
     module_to_wat m wat_file_name;
     module_to_wasm m wasm_file_name;
     assume (try Valid.check_module m; true with (Valid.Invalid (r,str)) -> false);
     let res = iter_helper n m in
     if res
     then res
     else (module_to_wat m wat_file_name; res))
;;

QCheck_runner.run_tests_main
  [
    shrink_test
  ] ;;
