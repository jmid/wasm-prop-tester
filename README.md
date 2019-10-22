# WebAssembly Property-Based Testing

This project implements a generator of arbitrary WebAssembly programs
for the purpose of property-based testing of different WebAssembly engines.

The code piggybacks on the [WebAssembly reference interpreter's](https://github.com/WebAssembly/spec) 
abstract syntax tree (in OCaml).


## External Dependencies

* A `timeout` command
  (on Mac OSX, install coreutils, then, e.g., `ln -s /opt/local/bin/gtimeout ~/bin/timeout`

* OCaml and the [QCheck](https://github.com/c-cube/qcheck) package

* WebAssembly reference interpreter, from spec: [WebAssembly spec](https://github.com/WebAssembly/spec)
  Assuming it is installed in the `spec` sub-directory:
```
export PATH="$PATH:$PWD/spec/interpreter"
```

* WebAssembly binary toolkit: [WebAssembly wabt](https://github.com/WebAssembly/wabt)
  Assuming it is installed in the `wabt` sub-directory:
```
export PATH="$PATH:$PWD/wabt/bin"
```

* JavaScript (engine) Version Updater: [jsvu](https://github.com/GoogleChromeLabs/jsvu)
```
export PATH="$PATH:$HOME/.jsvu"
```

* Eshost CLI: [eshost-cli](https://github.com/bterlson/eshost-cli)
  (no longer a requirement)


## Issues Found

* SpiderMonkey: [Crash when start function is added to table](https://bugzilla.mozilla.org/show_bug.cgi?id=1545086)  (new)
* JavaScriptCore: [Wasm engine segmentation fault](https://bugs.webkit.org/show_bug.cgi?id=202786)
* Chakra: [Compile-time rejection of unreachable tee_local](https://github.com/microsoft/ChakraCore/issues/6185)  (known)
