all: target/ target/test target/test2

target/test: graph.mli graph.ml conflict.mli conflict.ml test.ml
	ocamlopt.opt $^ -o $@

target/test2: tac.mli tac.ml ast.mli ast.ml test2.ml
	ocamlopt.opt str.cmxa $^ -o $@

target/:
	mkdir -p target/

clean:
	rm -f *.cmi *.cmx *.o
