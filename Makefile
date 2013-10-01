all : puzzle.ml
	ocamlopt -o puzzle util.mli util.ml puzzle.ml

clean : 
	rm -rf puzzle *.cmo *.cmi *.o *.cmx

