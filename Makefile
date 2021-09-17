.PHONY: all
all:
	@dune build

.PHONY: isla-lang
isla-lang:
	@dune build -p isla-lang

.PHONY: isla-lang.coq
isla-lang.coq:
	@dune build -p isla-lang.coq

.PHONY: pdf
pdf: isla_lang_quotiented.pdf isla_lang_unquotiented.pdf

isla_lang_quotiented.pdf: isla_lang.ott
	@dune build $@

isla_lang_unquotiented.pdf: isla_lang.ott
	@dune build $@

.PHONY: clean
clean:
	@dune clean

.PHONY: realclean
realclean: clean
	@rm -rf *.pdf

.PHONY: install
install: opam
	@dune install

.PHONY: uninstall
uninstall: opam
	@dune uninstall

.PHONY: format
format:
	@dune build @fmt
