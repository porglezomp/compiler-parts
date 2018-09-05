DOT=$(wildcard target/*.dot)
SVG=$(DOT:%.dot=%.svg)

all: target/ target/test target/test2 $(SVG)

target/test: graph.mli graph.ml conflict.mli conflict.ml test.ml
	ocamlopt.opt $^ -o $@

target/test2: tac.mli tac.ml ast.mli ast.ml test2.ml
	ocamlopt.opt str.cmxa $^ -o $@

target/%.svg: target/%.dot
	dot $< -Tsvg > $@

target/:
	mkdir -p target/

clean:
	rm -f *.cmi *.cmx *.o
