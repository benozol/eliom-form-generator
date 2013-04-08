PKG_NAME := deriving-eliom-form
ELIOMC = eliomc
JS_OF_ELIOM = js_of_eliom
OCAMLC = ocamlfind ocamlc

SERVER_DIR = server
CLIENT_DIR = client

export ELIOM_TYPE_DIR = _server
export ELIOM_SERVER_DIR = _server
export ELIOM_CLIENT_DIR = _client

OPTS := -thread -package deriving-ocsigen
PA_COPTS := -package deriving-ocsigen.syntax,camlp4.quotations.o -syntax camlp4o
PA_COPTS_TC := -package deriving-ocsigen.syntax_tc,camlp4.quotations.o -syntax camlp4o

.PHONY: all clean install uninstall

all: pa_deriving_Form.cma pa_deriving_Form_tc.cma $(ELIOM_SERVER_DIR)/deriving_Form.cmo $(ELIOM_CLIENT_DIR)/deriving_Form.cmo

$(TYPE_DIR)/%.type_mli: %.eliom
	$(ELIOMC) -infer -package js_of_ocaml $^

$(ELIOM_SERVER_DIR)/%.cmo: %.eliom $(TYPE_DIR)/%.type_mli
	$(ELIOMC) -c $(OPTS) $<

$(ELIOM_CLIENT_DIR)/%.cmo: %.eliom $(TYPE_DIR)/%.type_mli
	$(JS_OF_ELIOM) -c $(OPTS) $<

%.cmo: %.ml
	$(OCAMLC) $(PA_COPTS) -c -o $@ $<

%_tc.cmo: %_tc.ml
	$(OCAMLC) $(PA_COPTS_TC) -c -o $@ $<

pa_deriving_Form_tc.cmo: pa_deriving_Form_base.cmo
pa_deriving_Form.cmo: pa_deriving_Form_base.cmo

pa_deriving_Form.cma: pa_deriving_Form_base.cmo pa_deriving_Form.cmo
	$(OCAMLC) -a -o $@ $^

pa_deriving_Form_tc.cma: pa_deriving_Form_base.cmo pa_deriving_Form_tc.cmo
	$(OCAMLC) -a -o $@ $^

clean:
	rm -rf *.cmi *.cmo *.cma $(ELIOM_TYPE_DIR) $(ELIOM_SERVER_DIR) $(ELIOM_CLIENT_DIR)

install:
	ocamlfind install $(PKG_NAME) META pa_deriving_Form.cma pa_deriving_Form_tc.cma
	cp -r $(ELIOM_SERVER_DIR) `ocamlfind query $(PKG_NAME)`/$(SERVER_DIR)
	cp -r $(ELIOM_CLIENT_DIR) `ocamlfind query $(PKG_NAME)`/$(CLIENT_DIR)

uninstall:
	rm -rf `ocamlfind query $(PKG_NAME)`/$(SERVER_DIR) `ocamlfind query $(PKG_NAME)`/$(CLIENT_DIR)
	ocamlfind remove $(PKG_NAME)
