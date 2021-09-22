.PHONY: all
all:
	@dune build

.PHONY: clean
clean:
	@dune clean

.PHONY: install
install:
	@dune install

.PHONY: uninstall
uninstall:
	@dune uninstall

.PHONY: apply_header
apply_header:
	$(MAKE) clean
	headache -c etc/headache_config -h LICENSE isla_lang.ott isla_lang.ml main.ml

doc/isla_lang_quotiented.pdf:
	@dune build _build/default/isla_lang_quotiented_generated.pdf
	@cp _build/default/isla_lang_quotiented_generated.pdf $@
	@chmod 755 $@

doc/isla_lang_unquotiented.pdf:
	@dune build _build/default/isla_lang_unquotiented_generated.pdf
	@cp _build/default/isla_lang_unquotiented_generated.pdf $@
	@chmod 755 $@

.PHONY: update_pdfs
update_pdfs: doc/isla_lang_quotiented.pdf doc/isla_lang_unquotiented.pdf
