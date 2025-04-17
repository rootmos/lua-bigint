CURRENT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
ROOT ?= $(realpath $(CURRENT_DIR)/..)
TOOLS ?= $(ROOT)/tools
FETCH ?= $(TOOLS)/fetch
export FETCH_MANIFEST ?= $(CURRENT_DIR)/deps.json

all: lua luaunit

luaunit: luaunit.tar.gz
	mkdir $@
	tar xvf $< --strip-components=1 -C $@ --wildcards '*/luaunit.lua' '*/LICENSE.txt'

luaunit.tar.gz: $(FETCH_MANIFEST)
	$(FETCH) download $@

lua:
	$(MAKE) -C lua

deepclean:
	rm -rf luaunit.tar.gz luaunit
	make -C lua clean

.PHONY: all
.PHONY: lua
.PHONY: deepclean
