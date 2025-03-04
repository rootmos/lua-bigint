test: deps
	./run -t

run: deps
	./run

deps:
	$(MAKE) -C deps

.PHONY: test
.PHONY: deps
