.DEFAULT_GOAL := help

PYTHON ?= python3
OPAM_SWITCH ?=
ARCH_OUTPUT ?= dist/arch
SCRATCH_ROOT ?= $(HOME)/.cache/finances/build

SWITCH_ARG = $(if $(strip $(OPAM_SWITCH)),--switch "$(OPAM_SWITCH)")
OPAM_EXEC = opam exec $(SWITCH_ARG) --
ARCH_BUILD = $(PYTHON) packaging/arch/build.py $(SWITCH_ARG) --output "$(ARCH_OUTPUT)" --scratch-root "$(SCRATCH_ROOT)"

.PHONY: help build test fmt pkgbuild package

help:
	@echo 'make build       Build the CLI and libraries'
	@echo 'make test        Run the regression suite'
	@echo 'make fmt         Apply ocamlformat through Dune'
	@echo 'make pkgbuild    Generate dist/arch/PKGBUILD and its native payload'
	@echo 'make package     Generate the PKGBUILD and build the Arch package'
	@echo 'Options: OPAM_SWITCH=5.5.0 ARCH_OUTPUT=dist/arch SCRATCH_ROOT=...'

build:
	@mkdir -p "$(SCRATCH_ROOT)"
	TMPDIR="$(SCRATCH_ROOT)" $(OPAM_EXEC) dune build @all

test:
	@mkdir -p "$(SCRATCH_ROOT)"
	TMPDIR="$(SCRATCH_ROOT)" $(OPAM_EXEC) dune runtest

fmt:
	@mkdir -p "$(SCRATCH_ROOT)"
	TMPDIR="$(SCRATCH_ROOT)" $(OPAM_EXEC) dune fmt

pkgbuild:
	$(ARCH_BUILD) --prepare-only

package:
	$(ARCH_BUILD)
