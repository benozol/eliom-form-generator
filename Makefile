PKG_NAME := eliom-form-generator


WARNINGS = -w +0..29-4
ELIOMC = eliomc $(WARNINGS)
JS_OF_ELIOM = js_of_eliom $(WARNINGS)
OCAMLC = ocamlfind ocamlc $(WARNINGS)

SERVER_DIR = server
CLIENT_DIR = client

export ELIOM_TYPE_DIR = _types
export ELIOM_SERVER_DIR = _server
export ELIOM_CLIENT_DIR = _client

FILES = files

OPTS := -thread -package deriving,deriving.syntax,deriving-typerepr -ppopt pa_deriving_Typerepr.cmo
PA_COPTS := -package deriving,deriving.syntax,js_of_ocaml.deriving.syntax,camlp4.quotations.o,deriving-typerepr.syntax

.PHONY: all clean install uninstall depend

SOURCE_FILES=eliom_form_generator.eliom eliom_form_generator.eliomi
cmo_files=$(patsubst %.ml,%.cmo,$(patsubst %.eliom,%.cmo,$(shell eliomdep $(1) $(PA_COPTS) -sort $(SOURCE_FILES))))

all: META $(ELIOM_CLIENT_DIR)/eliom_form_generator.cmo $(ELIOM_SERVER_DIR)/eliom_form_generator.cmo

test: test.ml
	ocamlfind c -linkpkg -thread -syntax camlp4o -package ocsigenserver,ocsigenserver.ext.ocsipersist-sqlite,eliom.server,js_of_ocaml.deriving.syntax,deriving-typerepr.syntax,deriving-typerepr -I _server eliom_form_generator.cmo -o $@ $<

$(ELIOM_TYPE_DIR)/%.type_mli: %.eliom
	$(ELIOMC) -infer $(PA_COPTS) $<

$(ELIOM_SERVER_DIR)/%.cmo: %.eliom
	$(ELIOMC) -annot -c $(OPTS) $(PA_COPTS) $<

$(ELIOM_SERVER_DIR)/%.cmi: %.eliomi
	$(ELIOMC) -annot -c $(OPTS) $(PA_COPTS) $<

$(ELIOM_SERVER_DIR)/%.cmo: %.ml
	$(ELIOMC) -annot $(PA_COPTS) -c $<

$(ELIOM_CLIENT_DIR)/%.cmo: %.eliom
	$(JS_OF_ELIOM) -annot -c $(OPTS) $(PA_COPTS) $<

$(ELIOM_CLIENT_DIR)/%.cmi: %.eliomi
	$(JS_OF_ELIOM) -annot -c $(OPTS) $(PA_COPTS) $<

$(ELIOM_CLIENT_DIR)/%.cmo: %.ml
	$(JS_OF_ELIOM) -annot $(PA_COPTS) -c $<

ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),depend)
    include .depend
endif
endif
endif

.depend:
	eliomdep -server $(PA_COPTS) $(SOURCE_FILES) > .depend
	eliomdep -client $(PA_COPTS) $(SOURCE_FILES) >> .depend

depend:
	eliomdep -server $(PA_COPTS) $(SOURCE_FILES) > .depend
	eliomdep -client $(PA_COPTS) $(SOURCE_FILES) >> .depend

clean:
	rm -rf *.cmi *.cmo *.cma $(ELIOM_TYPE_DIR) $(ELIOM_SERVER_DIR) $(ELIOM_CLIENT_DIR) META
distclean: clean
	rm -rf .depend

META: META.in Makefile .depend
	sed -e 's/@@SERVER_CMO_FILES@@/$(call cmo_files,-server)/g' \
            -e 's/@@CLIENT_CMO_FILES@@/$(call cmo_files,-client)/g' \
	  $< > $@

install: all
	ocamlfind install $(PKG_NAME) META
	cp -r $(FILES) `ocamlfind query $(PKG_NAME)`/$(FILES)
	cp -r $(ELIOM_SERVER_DIR) `ocamlfind query $(PKG_NAME)`/$(SERVER_DIR)
	cp -r $(ELIOM_CLIENT_DIR) `ocamlfind query $(PKG_NAME)`/$(CLIENT_DIR)

uninstall:
	 rm -rf `ocamlfind query $(PKG_NAME)`/$(SERVER_DIR) \
	        `ocamlfind query $(PKG_NAME)`/$(CLIENT_DIR) \
	        `ocamlfind query $(PKG_NAME)`/$(FILES)
	ocamlfind remove $(PKG_NAME)
