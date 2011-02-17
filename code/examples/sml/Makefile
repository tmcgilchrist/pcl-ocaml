
ML = Symbol.mli Symbol.ml Ast.mli Ast.ml Parser.mli Parser.ml 

all:
	ocamlc -a -I ../.. ../../pcl.cma $(ML) -o sml.cma

debug:
	ocamlc -g -a -I ../.. ../../pcl.cma $(ML) -o sml.cma

clean:
	rm -f *.cmo *.cmx *.cmi *.cma *.cmxa *.a *.o
