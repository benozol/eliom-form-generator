NAME := deriving_Form
OCAMLC := ocamlfind c
OPTS := -thread -package deriving-ocsigen
PA_COPTS := -package deriving-ocsigen.syntax,camlp4.quotations.o -syntax camlp4o
PA_COPTS_TC := -package deriving-ocsigen.syntax_tc,camlp4.quotations.o -syntax camlp4o

all: pa_deriving_Form_base.cmo pa_deriving_Form.cmo pa_deriving_Form_tc.cmo server/deriving_Form.cmo # client/deriving_Form.cmo

server/deriving_Form.cmo: deriving_Form.ml | server
	$(OCAMLC) -c -o $@ $(OPTS) -package eliom.server deriving_Form.ml

# client/deriving_Form.cmo: deriving_Form.ml | server
# 	$(OCAMLC) -c -o deriving_Form.cmo $(OPTS) -package eliom.client deriving_Form.ml

%.cmo: %.ml
	$(OCAMLC) $(PA_COPTS) -c -o $@ $<

%_tc.cmo: %_tc.ml
	$(OCAMLC) $(PA_COPTS_TC) -c -o $@ $<

pa_deriving_Form_tc.cmo: pa_deriving_Form_base.cmo
pa_deriving_Form.cmo: pa_deriving_Form_base.cmo

server client:
	mkdir -p $@

clean:
	rm -rf *.cmi *.cmo server # client

install:
	ocamlfind install $(NAME) META pa_deriving_Form_base.cmo pa_deriving_Form.cmo pa_deriving_Form_tc.cmo
	cp -r server `ocamlfind query $(NAME)`

uninstall:
	rm -rf `ocamlfind query $(NAME)`/server	# `ocamlfind query $(NAME)`/client
	ocamlfind remove $(NAME)
