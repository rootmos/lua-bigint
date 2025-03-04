CURRENT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
ROOT ?= $(realpath $(CURRENT_DIR)/..)
TOOLS ?= $(ROOT)/tools
FETCH ?= $(TOOLS)/fetch
export FETCH_MANIFEST ?= $(CURRENT_DIR)/deps.json

all: luaunit

luaunit: luaunit.tar.gz
	mkdir $@
	tar xvf $< --strip-components=1 -C $@ --wildcards '*/luaunit.lua' '*/LICENSE.txt'

luaunit.tar.gz: $(FETCH_MANIFEST)
	$(FETCH) download $@

deepclean:
	rm -rf luaunit.tar.gz luaunit

.PHONY: all
.PHONY: deepclean
