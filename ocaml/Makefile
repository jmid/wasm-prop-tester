main:
	ocamlbuild -package qcheck,wasm,bigarray src/main.native

validate:
	ocamlbuild -package qcheck,wasm,bigarray src/validate.native

all:
	ocamlbuild -package qcheck,wasm,bigarray -no-hygiene src/generator.cma
	ocamlbuild -package qcheck,wasm,bigarray -no-hygiene src/generator.byte
	ocamlbuild -package qcheck,wasm,bigarray -no-hygiene src/generator.native

cov:
	# ocamlbuild -package qcheck,bigarray -no-hygiene -I includes_4.04.0spacetime/ src/generator.native
	ocamlbuild -cflag -g -package qcheck,wasm,bigarray,bisect_ppx -no-hygiene -I includes/ src/generator.native
	ocamlbuild -package qcheck,wasm,bigarray,bisect_ppx -no-hygiene -I includes/ src/generator.byte
	ocamlbuild -package qcheck,wasm,bigarray,bisect_ppx -no-hygiene -I includes/ src/generator.cma

clean:
	ocamlbuild -clean
