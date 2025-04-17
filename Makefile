tests: deps
	$(MAKE) -C tests
	./run -t
	$(MAKE) -C examples

run: deps
	./run

deps:
	$(MAKE) -C deps

doc:
	$(MAKE) -C doc

.PHONY: tests run
.PHONY: deps
.PHONY: doc
