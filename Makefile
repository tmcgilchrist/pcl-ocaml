
MODULES = LazyList Pos Token Error Prim Combinator Expr CharParse Language

ML  = $(MODULES:=.ml)

all:
	ocamlc -a $(ML) -o pcl.cma
opt:
	ocamlopt -a $(ML) -o pcl.cmxa
debug:
	ocamlc -g -a $(ML) -o pcl.cma
prof:
	ocamlcp -a $(ML) -o pcl.cma
fullprof:
	ocamlcp -p a -a $(ML) -o pcl.cma
gprof:
	ocamlopt -p -a $(ML) -o pcl.cmxa
check: all
	cd test; make; ./check; cd ..

clean:
	rm -f *.cmo *.cmx *.cmi *.cma *.cmxa *.a *.o
