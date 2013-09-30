all : puzzle.ml
	ocamlc -o puzzle puzzle.ml

clean : 
	rm -rf puzzle *.cmo *.cmi

