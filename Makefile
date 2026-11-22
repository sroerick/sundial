# wyo: sundial/ tree (the fleet cron / schedule substrate). Copy of record:
# scripts/monorepo-sundial.Makefile, promoted to sundial/Makefile by dune
# (@ci-dirs). Deploy job: build, then install the rc.d unit.
SUNDIAL_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

.PHONY: all build test install

all: build

build:
	dune build --root=$(SUNDIAL_DIR)

test:
	dune runtest --root=$(SUNDIAL_DIR)

install: all
	@echo "sundial: install rc.d unit from scripts/rc.d"
