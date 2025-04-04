CURRENT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
ROOT = $(CURRENT_DIR)/root

EXEs = $(ROOT)/lua32/bin/lua $(ROOT)/lua64/bin/lua

build: $(EXEs)
$(EXEs):
	./build

clean:
	rm -rf root

.PHONY: build clean
