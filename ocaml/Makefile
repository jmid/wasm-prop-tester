all:
	# ocamlbuild -package qcheck,bigarray -no-hygiene -I includes_4.04.0spacetime/ src/generator.native
	ocamlbuild -cflag -g -package qcheck,bigarray,bisect_ppx -no-hygiene -I includes/ src/generator.native
	ocamlbuild -package qcheck,bigarray,bisect_ppx -no-hygiene -I includes/ src/generator.byte
	ocamlbuild -package qcheck,bigarray,bisect_ppx -no-hygiene -I includes/ src/generator.cma

clean:
	ocamlbuild -clean
