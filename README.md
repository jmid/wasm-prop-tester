# Property-Based Testing of WebAssembly 

[![Build Status](https://travis-ci.com/jmid/wasm-prop-tester.svg?branch=master)](https://travis-ci.com/jmid/wasm-prop-tester)

This project implements a stack-driven generator of arbitrary WebAssembly programs
described in the paper

> Árpád Perényi and Jan Midtgaard  
> Stack-Driven Program Generation of WebAssembly  
> APLAS'2020  
> https://janmidtgaard.dk/papers/Perenyi-Midtgaard%3aAPLAS20.pdf

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

* The WebAssembly reference interpreter from the [WebAssembly specification](https://github.com/WebAssembly/spec).  
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

* [Node.js](https://nodejs.org/en/) to help transform a generated `.wasm` file into an independent JavaScript-file
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
However we have also used the [WebAssembly binary toolkit (wabt)](https://github.com/WebAssembly/wabt)
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

If `jsc` complains about an unsupported locale (and thus producing a different output)
```
 export LC_ALL="C"
```
should make it behave.


## Examples

Here is first an example of successful test run:
```
 $ ./main.native -v
 random seed: 169405920
 generated error fail pass / total     time test name
 [✓]  100    0    0  100 /  100    56.8s compare engines
 ================================================================================
 success (ran 1 tests)
```
In this case in less than 1 minute 100 modules were generated and had their output
compared across the reference interpreter and the four JavaScript engines
(V8, JavaScriptCore, SpiderMonkey, Chakra) without finding any disagreements.

We can find the latest generated module in `tmp/tmp_module.{wat,wasm}`,
its embeddings in `tmp/tmp_{v8,sm,jsc,ch}.js`, and the (normalized)
outputs in `tmp/tmp_{v8,sm,jsc,ch,spec}`.



Here's another example finding a disagreement:
```
 $ ./main.native -v
 random seed: 252860053
 generated error fail pass / total     time test name
 [✗]   27    0    1   26 /  100    72.8s ref interpret vs. js-engines

 --- Failure --------------------------------------------------------------------

 Test ref interpret vs. js-engines failed (27 shrink steps):

 (module
   (memory $0 4)
   (data
     0
     (offset (i32.const 261_218))
     "\69\02\96\22\d8\19\8c\be\82\d1\61\67\ba\09\86\4a"
     "\c4\53\dc\9f\7e\98\df\20\78\c2\11\dc\e4\8d\f6\f9"
     "\d4\4b\3c\4b\c3\c5\03\c8\c1\16\55\03\a8\9f\78\52"

     ...

     "\bd\27\a7\dd\5b\39\b4\20\42\32\b0\c9\6a\ef\23\fe"
     "\0a\ae\86\cd\66\f4\ed\02\11\43\7e\d2\47\95\0c\2d"
     "\38\16\29\35\25\87\86\09\cb\96\a1\12\06\d7\5c"
   )
 )

 ================================================================================
 failure (1 tests failed, 0 tests errored, ran 1 tests)
```
Here, on the 27th generated module a disagreement was found. Afterwards it took
27 shrinking steps (reductions or simplications) to cut the counterexample down.
In total it took a little more than one minute.

Since the last (unsuccessful) shrinking step overwrote the previous
output files, we save the previous disagreeing run in separate files.
The latest generated module illustrating disagreement is stored in `tmp/prev.{wat,wasm}`,
its embeddings in `tmp/prev_{v8,sm,jsc,ch}.js`, and the (normalized)
outputs in `tmp/prev_{v8,sm,jsc,ch,spec}`.

We can thus observe the output disagreement:
```
 $ cat tmp/prev_{ch,jsc,sm,v8,spec}
 LinkError data segment does not fit memory
 LinkError data segment does not fit memory
 RuntimeError out of bounds memory access
 RuntimeError data segment does not fit memory
 LinkError data segment does not fit memory
```
Here we see that the counterexample triggers a `RuntimeError` exception
on SpiderMonkey and V8 but a `LinkError` exception on Chakra, JavaScriptCore,
and in the reference interpreter. Furthermore, we failed to normalize
SpiderMonkey's exception message to something agreeing with the rest.

Printing the unnormalized messages carried by each exception shows a
range of detail, revealing how it may be tricky to separate this
particular SpiderMoney error from others (from shortest to longest):

- SM: `index out of bounds`
- CH: `Data segment is out of range`
- V8: `WebAssembly.Instance(): data segment is out of bounds`
- JSC: `Invalid data segment initialization: segment of 927 bytes memory of 262144 bytes, at offset 261218, segment writes outside of memory (evaluating 'new WebAssembly.Instance(new WebAssembly.Module(buffer), importObject)')`


## Issues Found

* SpiderMonkey: [Crash when start function is added to table](https://bugzilla.mozilla.org/show_bug.cgi?id=1545086)  (new, fixed)
* JavaScriptCore: [Wasm engine segmentation fault](https://bugs.webkit.org/show_bug.cgi?id=202786)
* JavaScriptCore: [Register allocation crash? FATAL: No color for %ftmp0](https://bugs.webkit.org/show_bug.cgi?id=209294)  (known, already fixed)
* JavaScriptCore: [br_table behavior](https://bugs.webkit.org/show_bug.cgi?id=209333) (new,confirmed)
* Chakra: [Compile-time rejection of unreachable tee_local](https://github.com/microsoft/ChakraCore/issues/6185)  (known)


### Other inconsistencies:

* JavaScriptCore: [Different exception name properties](https://bugs.webkit.org/show_bug.cgi?id=204054)
* Different stack overflow exceptions (described in the paper, not reported)
* Different data segment exceptions (described in the paper and above, not reported)


If you find more errors using the generator please let us know.
