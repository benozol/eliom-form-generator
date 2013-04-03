NAME := deriving-eliom-form
ELIOMC = eliomc
JS_OF_ELIOM = js_of_eliom
OCAMLC = ocamlfind ocamlc

TYPE_DIR = .

OPTS := -thread -package deriving-ocsigen
PA_COPTS := -package deriving-ocsigen.syntax,camlp4.quotations.o -syntax camlp4o
PA_COPTS_TC := -package deriving-ocsigen.syntax_tc,camlp4.quotations.o -syntax camlp4o

all: pa_deriving_Form_base.cmo pa_deriving_Form.cmo pa_deriving_Form_tc.cmo server/deriving_Form.cmo client/deriving_Form.cmo

$(TYPE_DIR)/deriving_Form.type_mli: deriving_Form.eliom
	$(ELIOMC) -infer -o $@ -package js_of_ocaml $^

server/deriving_Form.cmo: deriving_Form.eliom $(TYPE_DIR)/deriving_Form.type_mli | server
	$(ELIOMC) -c -o $@ $(OPTS) -type-dir $(TYPE_DIR) -package js_of_ocaml deriving_Form.eliom

client/deriving_Form.cmo: deriving_Form.eliom $(TYPE_DIR)/deriving_Form.type_mli | server
	$(JS_OF_ELIOM) -c -o $@ $(OPTS) -type-dir $(TYPE_DIR) deriving_Form.eliom

%.cmo: %.ml
	$(OCAMLC) $(PA_COPTS) -c -o $@ $<

%_tc.cmo: %_tc.ml
	$(OCAMLC) $(PA_COPTS_TC) -c -o $@ $<

pa_deriving_Form_tc.cmo: pa_deriving_Form_base.cmo
pa_deriving_Form.cmo: pa_deriving_Form_base.cmo

server client:
	mkdir -p $@

clean:
	rm -rf *.cmi *.cmo *.type_mli server client

install:
	ocamlfind install $(NAME) META pa_deriving_Form_base.cmo pa_deriving_Form.cmo pa_deriving_Form_tc.cmo
	cp -r server client `ocamlfind query $(NAME)`

uninstall:
	rm -rf `ocamlfind query $(NAME)`/server `ocamlfind query $(NAME)`/client
	ocamlfind remove $(NAME)
