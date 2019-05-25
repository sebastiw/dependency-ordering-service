PROJECT = dependency-ordering-service
VERSION := $(shell git describe --always --dirty)
REBAR3 ?= $(CURDIR)/rebar3

.PHONY: all test
.PHONY: compile xref dialyzer eunit clean shell upgrade release
.PHONY: image

test: eunit xref dialyzer

compile xref dialyzer eunit clean shell upgrade release:
	@$(REBAR3) $@

image:
	@docker build -t $(PROJECT):$(VERSION) .

run:
	@docker run -d -p 8000:8000 $(PROJECT):$(VERSION)
