open Wasm
open QCheck
open Generator

let output_validates =
  Test.make ~name:"output validates" ~count:1000
  arb_module
  (function m ->
     (* try *)
       Valid.check_module m;
       true
     (*with (Valid.Invalid (r,str)) -> false *)
  )

let output_wabt_validates_int_conv =
  Test.make ~name:"output validates with wabt (int.conv)" ~count:500
  arb_module
  (function m ->
     (*module_to_wat m wat_file_name;*)
     module_to_wasm m wasm_file_name;
     Sys.command ("wasm-validate " ^ wasm_file_name ^ " > /tmp/error 2> /tmp/error ") = 0)

let output_wabt_validates_ext_conv =
  Test.make ~name:"output validates with wabt (ext.conv)" ~count:500
  arb_module
  (function m ->
     (* try *)
     module_to_wat m wat_file_name;
     Sys.command ("wat2wasm " ^ wat_file_name ^ " -o " ^ wasm_file_name) = 0 &&
     Sys.command ("wasm-validate " ^ wasm_file_name ^ " > /tmp/error 2> /tmp/error ") = 0)

let output_wabt_validates_with engine =
  Test.make ~name:("output validates with " ^ engine) ~count:100
  arb_module
  (function m ->
     (*module_to_wat m wat_file_name;*)
     module_to_wasm m wasm_file_name;
     Sys.command ("node javascript/createvalidation.js " ^ wasm_file_name ^ " > tmp/tmp_module.js") = 0 &&
     Sys.command (engine ^ " tmp/tmp_module.js > /tmp/tmp_out 2> /tmp/error ") = 0 &&
     Sys.command ("script/check_true.sh /tmp/tmp_out") = 0)
;;

QCheck_runner.run_tests_main
  [ output_validates;
    output_wabt_validates_int_conv;
    output_wabt_validates_ext_conv;
    (*  output_wabt_validates_with "ch"; *)
    output_wabt_validates_with "v8"; 
    output_wabt_validates_with "sm"; 
    output_wabt_validates_with "jsc";
  ]
