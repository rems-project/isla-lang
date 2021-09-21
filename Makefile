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
