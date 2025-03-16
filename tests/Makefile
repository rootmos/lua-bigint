run: lua-bigint.cabal
	stack run specs

build clean: lua-bigint.cabal
	stack $@

lua-bigint.cabal: package.yaml
	hpack

.PHONY: run build clean
