REBAR3 = rebar3
TSC = tsc

.PHONY: all
all: compile

.PHONY: help
help:
	@echo "Targets:"
	@echo "  all"
	@echo "  compile - compile all projects"
	@echo "  rebar3 - compile erlang rebar3 project"
	@echo "  ts - compile typescript project"

.PHONY: compile
compile: rebar3-compile ts

.PHONY: rebar3-compile
rebar3-compile:
	$(REBAR3) compile

.PHONY: ts
ts:
	$(TSC) --outDir apps/eradio/priv/htdocs/js/ --project apps/eradio/ts/ --pretty false
