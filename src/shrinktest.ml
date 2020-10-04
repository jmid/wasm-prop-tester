open Wasm
open QCheck
open Generator

let wat_shrunk_name = tmp_dir_name ^ "/" ^ "shrunk_module.wat";;

exception Done
let iter_helper n m =
  try
    let count = ref n in
    Shrinker.module_shrink m (fun m' ->
        module_to_wat m' (tmp_dir_name ^ "/shrunk_iter" ^ string_of_int !count ^ ".wat");
        if 0 = !count
        then raise Done
        else
          try
            (Valid.check_module m'; decr count)
          with e ->
            (try
               module_to_wat m' wat_shrunk_name;
             with e ->
               Printf.printf "oh no, handler raised exception\n"; flush stdout; raise e);
            raise e);
    true
  with Done -> true
     | _ -> false (* shrinking threw an exception *)

let shrink_test =
  Test.make ~name:"shrink test" ~count:1000 (*10000*)
    (pair
       (set_shrink Shrink.nil (arb_module))
       QCheck.small_nat
       (*(make ~print:Print.int ~shrink:Shrink.int (Gen.return 10))*)
    )
    (function (m,n) ->
       try
         module_to_wat m wat_file_name;
         module_to_wasm m wasm_file_name;
         assume (try Valid.check_module m; true
                 with (Valid.Invalid (r,str)) -> false
                    | _ -> false); (* check_module threw an exception *)
         let res = iter_helper n m in
         if res
         then res
         else (module_to_wat m wat_file_name; res)
       with (* iter_helper/module_to_wat/wasm threw exception *)
        _ -> false
    )
;;
QCheck_runner.run_tests_main [shrink_test]
