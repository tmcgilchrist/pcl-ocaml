
TESTS = PrimTest CharTest CombTest

ML  = Test.ml $(TESTS:=.ml) TestMain.ml

all:
	ocamlc -I .. ../pcl.cma $(ML) -o check

clean:
	rm -f *.cmo *.cmx *.cmi *.cma *.cmxa *.a *.o check
