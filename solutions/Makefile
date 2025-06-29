# Makefile - koch_snowflake
SRC := Grimaud_Koch_SnowFlake
FRACTAL := Koch_SnowFlake

.PHONY: clean

$(FRACTAL): $(SRC).ml
	ocamlfind ocamlopt -package graphics -linkpkg -o $@ $<

clean:
	rm $(FRACTAL) *.cmi *.cmx *.o *~
