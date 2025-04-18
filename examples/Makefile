RUNNER ?= ../run

tests:
	./bignat.calc | diff - bignat.output
	$(RUNNER) bignat.lua | diff - bignat.output

.PHONY: tests
