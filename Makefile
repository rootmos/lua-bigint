tests: deps
	$(MAKE) -C tests
	./run -t
	$(MAKE) -C examples

run: deps
	./run

deps:
	$(MAKE) -C deps

.PHONY: tests run
.PHONY: deps
