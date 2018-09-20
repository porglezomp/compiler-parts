DOT=$(wildcard target/*.dot)
SVG=$(DOT:%.dot=%.svg)

all: target/ target/test target/test2 target/test3 target/test4 $(SVG)

target/test: graph.mli graph.ml conflict.mli conflict.ml test.ml
	ocamlopt.opt $^ -o $@

target/test2: s.mli dom.mli dom.ml tac.mli tac.ml ast.mli ast.ml test2.ml
	ocamlopt.opt str.cmxa $^ -o $@

target/test3: s.mli cfg.mli cfg.ml dom.mli dom.ml test3.ml
	ocamlopt.opt -g $^ -o $@

target/test4: s.mli dom.mli dom.ml tac.mli tac.ml ast.mli ast.ml test4.ml
	ocamlopt.opt str.cmxa $^ -o $@

target/%.svg: target/%.dot
	dot $< -Tsvg > $@

target/:
	mkdir -p target/

clean:
	rm -f *.cmi *.cmx *.o
