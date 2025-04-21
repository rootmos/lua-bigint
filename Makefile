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

clean:
	$(MAKE) -C tests clean
	rm -f src/build-info.lua

.PHONY: tests run
.PHONY: deps
.PHONY: doc
.PHONY: clean
