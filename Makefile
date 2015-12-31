CC = ocamlfind ocamlc
EXEC = celaut
FILES = dimacs valuation minisat

ML = $(wildcard *.ml)
CMO = $(patsubst %.ml, %.cmo, $(ML))
CMI = $(patsubst %.ml, %.cmi, $(ML))

all: cleanfiles files $(EXEC) clean

files:
	touch $(FILES)
	chmod 777 $(FILES)

$(EXEC): $(CMO)
	$(CC) -o $@ unix.cma $^

celaut.cmi: celaut.ml
	$(CC) -i celaut.ml

%.cmi: %.mli
	$(CC) $<

%.cmo: %.ml $(CMI)
	$(CC) -c $<

clean:
	rm -f $(CMO) $(CMI)

cleanfiles:
	rm -f $(FILES)

mrproper: clean cleanfiles
	rm -f $(EXEC)
