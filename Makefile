all: build

.PHONY: build
build:
	rebar3 compile

.PHONY: clean
clean:
	rm -Rf deps
	rebar3 clean

.PHONY: test
test: all
	rebar3 fmt --check
	rebar3 xref
	rebar3 dialyzer

.PHONY: fresh
fresh:
	rm -rf _build

.PHONY: format
format: build
	rebar3 fmt
