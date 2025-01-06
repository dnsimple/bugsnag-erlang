REBAR:=$(shell which rebar3 || echo ./rebar3)
REBAR_URL:="https://s3.amazonaws.com/rebar3/rebar3"

all: build

$(REBAR):
	wget $(REBAR_URL) && chmod +x rebar3

.PHONY: build
build: $(REBAR)
	@$(REBAR) compile

.PHONY: clean
clean: $(REBAR)
	rm -Rf deps
	@$(REBAR) clean

.PHONY: test
test: $(REBAR) all
	@$(REBAR) fmt --check

.PHONY: fresh
fresh:
	rm -rf _build

.PHONY: format
format: build
	@$(REBAR) fmt
