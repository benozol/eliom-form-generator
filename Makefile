PKG_NAME := deriving-eliom-form

WARNINGS = -w +0..29
ELIOMC = eliomc $(WARNINGS)
JS_OF_ELIOM = js_of_eliom $(WARNINGS)
OCAMLC = ocamlfind ocamlc $(WARNINGS)

SERVER_DIR = server
CLIENT_DIR = client

export ELIOM_TYPE_DIR = _server
export ELIOM_SERVER_DIR = _server
export ELIOM_CLIENT_DIR = _client

FILES = files

OPTS := -thread -package deriving-ocsigen
PA_COPTS := -package deriving-ocsigen.syntax,js_of_ocaml.deriving.syntax,camlp4.quotations.o
PA_COPTS_TC := -package deriving-ocsigen.syntax_tc,js_of_ocaml.deriving.syntax_tc,camlp4.quotations.o

.PHONY: all clean install uninstall depend

SOURCE_FILES=$(wildcard *.eliom)
cmo_files=$(patsubst %.eliom,%.cmo,$(shell eliomdep $(1) -sort $(SOURCE_FILES)))

all: pa_deriving_Form.cma pa_deriving_Form_tc.cma $(ELIOM_SERVER_DIR)/deriving_Form.cmo $(ELIOM_CLIENT_DIR)/deriving_Form.cmo META

$(ELIOM_TYPE_DIR)/%.type_mli: %.eliom
	$(ELIOMC) -infer -package js_of_ocaml $(PA_COPTS) $<

$(ELIOM_SERVER_DIR)/%.cmo: %.eliom
	$(ELIOMC) -c $(OPTS) $(PA_COPTS) $<

$(ELIOM_CLIENT_DIR)/%.cmo: %.eliom
	$(JS_OF_ELIOM) -c $(OPTS) $(PA_COPTS) $<

%.cmo: %.ml
	$(OCAMLC) -syntax camlp4o $(PA_COPTS) -c -o $@ $<

%_tc.cmo: %_tc.ml
	$(OCAMLC) -syntax camlp4o $(PA_COPTS_TC) -c -o $@ $<

pa_deriving_Form_tc.cmo: pa_deriving_Form_base.cmo
pa_deriving_Form.cmo: pa_deriving_Form_base.cmo

pa_deriving_Form.cma: pa_deriving_Form_base.cmo pa_deriving_Form.cmo
	$(OCAMLC) -a -o $@ $^

pa_deriving_Form_tc.cma: pa_deriving_Form_base.cmo pa_deriving_Form_tc.cmo
	$(OCAMLC) -a -o $@ $^

ifneq ($(MAKECMDGOALS),distclean)
    include .depend
endif
.depend:
	eliomdep -server $(SOURCE_FILES) > .depend
	eliomdep -client $(SOURCE_FILES) >> .depend
depend: | .depend

clean:
	rm -rf *.cmi *.cmo *.cma $(ELIOM_TYPE_DIR) $(ELIOM_SERVER_DIR) $(ELIOM_CLIENT_DIR) META
distclean: clean
	rm -rf .depend

META: META.in Makefile .depend
	sed -e 's/@@SERVER_CMO_FILES@@/$(call cmo_files,-server)/g' \
            -e 's/@@CLIENT_CMO_FILES@@/$(call cmo_files,-client)/g' \
	  $< > $@

install: all
	ocamlfind install $(PKG_NAME) META pa_deriving_Form.cma pa_deriving_Form_tc.cma
	cp -r $(FILES) `ocamlfind query $(PKG_NAME)`/$(FILES)
	cp -r $(ELIOM_SERVER_DIR) `ocamlfind query $(PKG_NAME)`/$(SERVER_DIR)
	cp -r $(ELIOM_CLIENT_DIR) `ocamlfind query $(PKG_NAME)`/$(CLIENT_DIR)

uninstall:
	rm -rf `ocamlfind query $(PKG_NAME)`/$(SERVER_DIR) \
	       `ocamlfind query $(PKG_NAME)`/$(CLIENT_DIR) \
	       `ocamlfind query $(PKG_NAME)`/$(FILES)
	ocamlfind remove $(PKG_NAME)
