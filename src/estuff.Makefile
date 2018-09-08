REBAR = $(CURDIR)/tools/rebar3
ERL := $(shell command -v erl 2> /dev/null)
CFG_DIR := $(CURDIR)/config

ifndef ERL
$(error "Could not found Erlang/OTP ('erlc' command) installed on this system.")
endif


.PHONY: all compile shell docs test clean distclean


all: test docs

compile:
	@ $(REBAR) compile

shell: compile
	@ erl -pa $(shell ls -d _build/default/lib/*/ebin) -config $(CFG_DIR)/sys.config -args_file $(CFG_DIR)/vm.args -eval "catch code:load_file('{{name}}')"

docs: compile
	@ $(REBAR) as doc edoc

test:
	@ $(REBAR) ct && $(REBAR) dialyzer

clean:
	@ $(REBAR) clean

distclean: clean
	@ rm -rf _build rebar.lock doc
