# LIBX11 doit désigner le chemin d'accès de la librairie X11
LIBX11      	= /usr/X11R6/lib
EXEC        	= logo  

$(EXEC): logo_main.cmo
	ocamlc -custom -o $(EXEC) graphics.cma logo_lexer.cmo logo_parser.cmo logo_analyseur_lexical.cmo logo_analyseur_syntaxique.cmo logo_analyseur_semantique.cmo logo_dessine.cmo logo_main.cmo -ccopt -L$(LIBX11)

all : logo_main.cmo

logo_main.cmo : logo_analyseur_lexical.cmo logo_analyseur_syntaxique.cmo logo_analyseur_semantique.cmo logo_dessine.cmo
	ocamlc -c logo_main.ml

logo_dessine.cmo : logo_dessine.ml logo_dessine.cmi
	ocamlc -c logo_dessine.ml

logo_dessine.cmi : logo_dessine.mli
	ocamlc -c logo_dessine.mli

logo_analyseur_semantique.cmo : logo_types.cmo logo_analyseur_semantique.cmi 
	ocamlc -c logo_analyseur_semantique.ml

logo_analyseur_semantique.cmi : logo_analyseur_semantique.mli
	ocamlc -c logo_analyseur_semantique.mli

logo_analyseur_syntaxique.cmo : logo_types.cmo logo_analyseur_syntaxique.cmi  
	ocamlc -c logo_analyseur_syntaxique.ml

logo_analyseur_syntaxique.cmi : logo_analyseur_syntaxique.mli
	ocamlc -c logo_analyseur_syntaxique.mli

logo_analyseur_lexical.cmo : logo_analyseur_lexical.ml logo_types.cmo logo_lexer.cmo logo_parser.cmo logo_analyseur_lexical.cmi 
	ocamlc -c logo_analyseur_lexical.ml

logo_analyseur_lexical.cmi : logo_types.cmo logo_analyseur_lexical.mli
	ocamlc -c logo_analyseur_lexical.mli

logo_parser.cmo :  logo_lexer.cmo logo_parser.ml logo_parser.cmi
	ocamlc -c logo_parser.ml

logo_parser.cmi : logo_parser.ml logo_parser.mli
	ocamlc -c logo_parser.mli

logo_parser.ml : logo_parser.mly
	ocamlyacc logo_parser.mly

logo_lexer.cmo : logo_lexer.ml logo_parser.cmi
	ocamlc -c logo_lexer.ml

logo_lexer.ml : logo_lexer.mll
	ocamllex logo_lexer.mll

logo_types.cmo : logo_types.ml
	ocamlc -c logo_types.ml

clean:
	\rm -f *.cmo *.cmi logo_lexer.ml logo_parser.ml $(EXEC)
