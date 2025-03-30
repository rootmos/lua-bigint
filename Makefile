tests: deps
	$(MAKE) -C tests
	./run -t

run: deps
	./run

deps:
	$(MAKE) -C deps

.PHONY: tests run
.PHONY: deps
