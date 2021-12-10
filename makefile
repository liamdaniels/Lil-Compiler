MAIN := lilc
MODULES := ast ir ir_io lexer parser main 
OBJECTS := $(MODULES:=.cmo)

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

$(MAIN): $(OBJECTS)
	ocamlc $^ -o $@ 

parser.ml: parser.mly
	ocamlyacc -q $<

parser.mli: parser.mly
	ocamlyacc -q $<

lexer.ml: lexer.mll
	ocamllex -q $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN)

ast.cmo :
lexer.cmo : parser.cmi
main.cmo : parser.cmi lexer.cmo 
parser.cmo : ast.cmo 
parser.cmi : ast.cmo
