CURRENT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
ROOT ?= $(realpath $(CURRENT_DIR)/..)

export LUA_BITS ?= 64
export PKG_CONFIG_PATH = $(ROOT)/deps/lua/root/lua$(LUA_BITS)/lib/pkgconfig

run: prepare
	stack run specs

build clean: prepare
	stack $@

prepare: lua-bigint.cabal bits

deps: prepare lua-bigint.cabal stack.yaml
	stack build --only-dependencies

lua-bigint.cabal: package.yaml
	stack exec hpack

LUA_BITS_FLAG = .lua_bits
bits: $(LUA_BITS_FLAG)
ifeq ($(file < $(LUA_BITS_FLAG)),)
$(LUA_BITS_FLAG):
	echo "$(LUA_BITS)" >$@
else ifneq ($(file < $(LUA_BITS_FLAG)),$(LUA_BITS))
$(LUA_BITS_FLAG): force-rebuild-lua
	echo "$(LUA_BITS)" >$@
else:
$(LUA_BITS_FLAG):
endif

force-rebuild-lua: FORCE
	stack exec -- ghc-pkg unregister --force lua

.PHONY: run build clean
.PHONY: prepare deps
.PHONY: bits force-rebuild-lua
FORCE:
