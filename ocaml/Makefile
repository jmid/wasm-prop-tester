all:
	ocamlbuild -package qcheck,bigarray -no-hygiene -I includes/ src/generator.native
	ocamlbuild -package qcheck,bigarray -no-hygiene -I includes/ src/generator.byte
	ocamlbuild -package qcheck,bigarray -no-hygiene -I includes/ src/generator.cma

clean:
	ocamlbuild -clean
