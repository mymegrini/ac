CC = ocamlfind ocamlc
EXEC = celaut

ML = $(wildcard *.ml)
CMO = $(patsubst %.ml, %.cmo, $(ML))
CMI = $(patsubst %.ml, %.cmi, $(ML))

all: $(EXEC)

$(EXEC): $(CMO)
	$(CC) -o $@ $^

celaut.cmi: celaut.ml
	$(CC) -i celaut.ml

%.cmi: %.mli
	$(CC) $<

%.cmo: %.ml $(CMI)
	$(CC) -c $<

Pgraphics.cmo: Pgraphics.ml $(CMI)
	$(CC) -c Pgraphics.ml graphics.cma $1 $2


clean:
	rm -f $(CMO) $(CMI)

mrproper: clean
	rm -f $(EXEC)
