.PHONY: run
run: lua-bigint.cabal
	stack run

.PHONY: build
build: lua-bigint.cabal
	stack build

lua-bigint.cabal: package.yaml
	hpack
