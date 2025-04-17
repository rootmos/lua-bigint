CURRENT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
ROOT ?= $(realpath $(CURRENT_DIR)/..)
TOOLS ?= $(ROOT)/tools

INCLUDE ?= $(TOOLS)/include

all: $(ROOT)/README.md

$(ROOT)/README.md: README.in.md $(wildcard examples/*.lua examples/*.output)
	$(INCLUDE) "$<" "$@"

.PHONY: all
