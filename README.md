# Property-Based Testing of WebAssembly 

This project implements a stack-driven generator of arbitrary WebAssembly programs
described in the forthcoming paper

> Árpád Perényi and Jan Midtgaard  
> Stack-Driven Program Generation of WebAssembly  
> APLAS'2020

We run each generated WebAssembly (Wasm) program on the engines underlying Chrome, Firefox, Safari, and Edge
and on the [WebAssembly reference interpreter](https://github.com/WebAssembly/spec) 
and compare their output to help ensure consistent behaviour. The generator is stack-directed
to produce programs that will pass validation ("are type correct"). A custom stack-directed shrinker
reduces the often large random counterexample programs, without breaking validation.

Surprisingly this black-box generator approach found 2 crashing bugs,
despite browser vendor efforts to fuzz test the engines with
coverage-aware (gray-box) fuzzers.

The code piggybacks on the [WebAssembly reference interpreter's](https://github.com/WebAssembly/spec) 
abstract syntax tree (in OCaml) and uses the [QCheck library](https://github.com/c-cube/qcheck) for property-based testing (QuickCheck). 


## External Dependencies

* OCaml and the [QCheck](https://github.com/c-cube/qcheck) package

* WebAssembly reference interpreter, from the [WebAssembly spec](https://github.com/WebAssembly/spec).
  Install the `wasm` package using `ocamlfind`:
  ```
  cd spec/interpreter
  make
  make install
  ```

  Optionally set up `PATH` for reference interpreter (assuming it is installed in the `spec` sub-directory):
  ```
  export PATH="$PATH:$PWD/spec/interpreter"
  ```

* A `bash` shell with a `cmp` command for diffing log files of observed outputs

* A `timeout` command to break infinite loops
  (on Mac OSX: install coreutils, then, e.g., `ln -s /opt/local/bin/gtimeout ~/bin/timeout`

* [Node.js](https://nodejs.org/en/) to help transform a generated .wasm file into an independent JavaScript-file
  suitable for running on a barebones JavaScript engine:
  ```
  sudo npm install -g npm
  ```

* JavaScript (engine) Version Updater: [jsvu](https://github.com/GoogleChromeLabs/jsvu)
  ```
  sudo npm install -g jsvu
  jsvu --engines=chakra,javascriptcore,spidermonkey,v8
  export PATH="$PATH:$HOME/.jsvu"
  ```



### An Optional Dependency

The generator uses the reference interpreter for emitting the Wasm binary format (`.wasm`). 
However we have also used the WebAssembly binary toolkit: [WebAssembly wabt](https://github.com/WebAssembly/wabt)
to convert `.wat` to `.wasm`. This is a dependency to run our full internal testsuite.

Assuming wabt is installed in the `wabt` sub-directory:
```
 export PATH="$PATH:$PWD/wabt/bin"
```


## Running

We have tested the generator under both Linux and Mac OSX.   
With a recent OCaml installed, compiling should be as simple as:
```
 cd ocaml
 make
```

Once compiled and the `PATH` setup, you can generate 100 programs as follows:
```
 ./main.native -v
```

For fun, you can run a loop that continues restarting the test runner
until it finds a counterexample:
```
 while ./main.native -v; do :; done
```

## Issues Found

* SpiderMonkey: [Crash when start function is added to table](https://bugzilla.mozilla.org/show_bug.cgi?id=1545086)  (new, fixed)
* JavaScriptCore: [Wasm engine segmentation fault](https://bugs.webkit.org/show_bug.cgi?id=202786)
* JavaScriptCore: [Register allocation crash? FATAL: No color for %ftmp0](https://bugs.webkit.org/show_bug.cgi?id=209294)  (known, already fixed)
* JavaScriptCore: [br_table behavior](https://bugs.webkit.org/show_bug.cgi?id=209333) (new,confirmed)
* Chakra: [Compile-time rejection of unreachable tee_local](https://github.com/microsoft/ChakraCore/issues/6185)  (known)
