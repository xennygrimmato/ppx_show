all:
	ocamlbuild src/ppx_show.native example/example.byte

clean:
	ocamlbuild -clean
