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
