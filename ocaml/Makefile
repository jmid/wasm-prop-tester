all:
	ocamlbuild -package qcheck,bigarray,bisect_ppx -no-hygiene -I includes/ src/generator.native
	ocamlbuild -package qcheck,bigarray,bisect_ppx -no-hygiene -I includes/ src/generator.byte
	ocamlbuild -package qcheck,bigarray,bisect_ppx -no-hygiene -I includes/ src/generator.cma

clean:
	ocamlbuild -clean
