main:
	ocamlbuild -package qcheck,wasm src/main.native

stats:
	ocamlbuild -package qcheck,wasm src/stats.native

validate:
	ocamlbuild -package qcheck,wasm src/validate.native

shrinktest:
	ocamlbuild -package qcheck,wasm src/shrinktest.native

all:
	ocamlbuild -package qcheck,wasm -no-hygiene src/generator.cma

cov:
	# ocamlbuild -package qcheck,wasm,bigarray -no-hygiene -I includes_4.04.0spacetime/ src/generator.native
	ocamlbuild -use-ocamlfind -package qcheck,wasm,bisect_ppx src/main.native
	#ocamlbuild -use-ocamlfind -package qcheck,wasm,bisect_ppx src/generator.cma
	ocamlbuild -use-ocamlfind -package qcheck,wasm,bisect_ppx src/shrinktest.native

clean:
	ocamlbuild -clean
