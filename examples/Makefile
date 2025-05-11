RUNNER ?= ../run

tests: bignat.test bigint.test

%.test:
	./$*.calc | diff - $*.output
	$(RUNNER) $*.lua | diff - $*.output

.PHONY: tests
