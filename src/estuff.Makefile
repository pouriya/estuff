REBAR = $(CURDIR)/tools/rebar3
ERL := $(shell command -v erl 2> /dev/null)


ifndef ERL
	$(error "Could not found Erlang/OTP installed on this system.")
endif


.PHONY: all compile shell docs test clean distclean


all: compile

compile:
	@ $(REBAR) compile

shell:
	@ $(REBAR) shell

docs:
	@ $(REBAR) as doc edoc

test:
	@ $(REBAR) ct && $(REBAR) dialyzer

clean:
	@ $(REBAR) clean

distclean: clean
	@ rm -rf _build rebar.lock doc
