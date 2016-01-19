CC = ocamlfind ocamlc
EXEC = celaut
FILES = dimacs valuation minisat

ML = $(wildcard *.ml)
CMO = $(patsubst %.ml, %.cmo, $(ML))
CMI = $(patsubst %.ml, %.cmi, $(ML))

all: mrproper files $(EXEC) clean

files:
	touch $(FILES)
	chmod 777 $(FILES)

celaut: $(ML)
	$(CC) IO.mli
	$(CC) Libcelaut.mli
	$(CC) -c IO.ml
	$(CC) -c unix.cma IO.cmo Libcelaut.ml 
	$(CC) -o celaut IO.cmo unix.cma Libcelaut.cmo graphics.cma celaut.ml 

clean:
	rm -f *.cmx *.cmi *.o *.cmo

cleanfiles:
	rm -f $(FILES)

mrproper: clean cleanfiles
	rm -f $(EXEC)
