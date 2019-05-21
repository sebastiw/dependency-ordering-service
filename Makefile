PROJECT = dependency-ordering-service
VERSION := $(shell git describe --always --dirty)
REBAR3 ?= $(CURDIR)/rebar3

.PHONY: all test
.PHONY: compile xref dialyzer eunit clean shell upgrade release

test: eunit xref dialyzer

compile xref dialyzer eunit clean shell upgrade release:
	@$(REBAR3) $@
