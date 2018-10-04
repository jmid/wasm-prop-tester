
/*fetch('module.wasm').then(response =>
  response.arrayBuffer()
).then(bytes =>
  WebAssembly.instantiate(bytes)
).then(result => {
  console.log(result)
  console.log(result.instance.exports.factorial(5))
});*/

var web = WebAssembly;

//console.log(WebAssembly);

//(module (func (export "aexp") (result i32) (i32.mul (i32.const 878) (i32.mul (i32.sub (i32.const 997) (i32.const 9)) (i32.mul (i32.const 91) (i32.const 9406))))))

//var reader = new FileReader();

//opam init
//opam upgrade
//opam switch 4.07.0
//eval `opam config env`

var a = 4;

console.log(4)
