test: deps
	$(MAKE) -C tests
	./run -t

run: deps
	./run

deps:
	$(MAKE) -C deps

.PHONY: test run
.PHONY: deps
