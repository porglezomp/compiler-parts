all: target/ target/test

target/test: graph.mli graph.ml conflict.mli conflict.ml test.ml
	ocamlopt.opt $^ -o $@

target/:
	mkdir -p target/

clean:
	rm -f *.cmi *.cmx *.o
