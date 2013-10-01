all : puzzle.ml
	ocamlc -o puzzle util.mli util.ml puzzle.ml

clean : 
	rm -rf puzzle *.cmo *.cmi

