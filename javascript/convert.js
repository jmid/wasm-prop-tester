const fs = require('fs');

if (process.argv.length !== 4) {
    throw "missing filename or javascript engine name argument";
}

const buf = fs.readFileSync(process.argv[2]);
const b = new Uint8Array(buf);

const error_int_zero_div = 'integer division by zero';
const error_overflow = 'integer overflow';
const error_unreachable = 'unreachable';
const error_stack = 'stack';
const error_data_segment = 'data segment';

let program = ["let debug = debug || (arg => console.log('-->', arg));\n"];
program.push("let unrepresentable_re = /(unrepresentable)/i;\n");

switch(process.argv[3]) {
    case "ch":
        program.push("let zero_div_re = /(Division by zero)/i;\n");
        program.push("let overflow_re = /(overflow)/i;\n");;
        program.push("let unreachable_re = /(Unreachable Code)/i;\n");
        program.push("let stack_re = /(Out of stack space)/i;\n")
        program.push("let data_segment_re = /(Data segment is out of range)/i;\n");
      break;
    case "jsc":
        program.push("let zero_div_re = /(Division by zero)/i;\n");
        program.push("let overflow_re = /(Out of bounds)/i;\n");
        program.push("let unreachable_re = /(Unreachable code should not be executed)/i;\n");
        program.push("let stack_re = /(Maximum call stack size exceeded)/i;\n");
        program.push("let data_segment_re = /(segment writes outside of memory)/i;\n");
      break;
    case "sm":
        program.push("let zero_div_re = /(integer divide by zero)/i;\n");
        program.push("let overflow_re = /(overflow)/i;\n");
        program.push("let unreachable_re = /(unreachable executed)/i;\n");
        program.push("let stack_re = /(too much recursion)/i;\n");
        program.push("let data_segment_re = /(data segment does not fit in memory)/i;\n");
        break;
    case "v8":
        program.push("let zero_div_re = /(by zero)/i;\n");
        program.push("let overflow_re = /(float unrepresentable in integer range)/i;\n");
        program.push("let unreachable_re = /(unreachable)/i;\n");
        program.push("let stack_re = /(Maximum call stack size exceeded)/i;\n");
        program.push("let data_segment_re = /(data segment is out of bounds)/i;\n");
        break;
  } 

program.push("let importObject = { imports: { log: debug } };\n");
program.push("let buffer = new Uint8Array(\[");
program.push(b.toString());
program.push("]);\n");
program.push("try {\n");
program.push("     let m = new WebAssembly.Instance(new WebAssembly.Module(buffer), importObject);\n");
program.push("     debug(m.exports.runi32());\n");
program.push("     debug(m.exports.runf32());\n");
program.push("     debug(m.exports.runf64());\n");
program.push("} catch(e) {\n");
program.push("     if (zero_div_re.exec(e.message)) debug('" + error_int_zero_div + "')\n");
program.push("     else if (unreachable_re.exec(e.message)) debug('" + error_unreachable + "')\n");
program.push("     else if (stack_re.exec(e.message)) debug('" + error_stack + "')\n");
program.push("     else if (overflow_re.exec(e.message)) debug('" + error_overflow + "')\n");
program.push("     else if (unrepresentable_re.exec(e.message)) debug('" + error_overflow + "')\n");
program.push("     else if (data_segment_re.exec(e.message)) debug('" + error_data_segment + "')\n");
program.push("     else debug(e)\n");
program.push("}");

console.log(program.join(""));
