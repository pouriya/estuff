REBAR                        = $(CURDIR)/tools/rebar3
ERL                         := $(shell command -v erl 2> /dev/null)
CFG_DIR                      = $(CURDIR)/config
READ_COVERAGE_SUMMARY_SCRIPT = 'BEGIN {FS="[<>]"; print_headers = 0} $$2 == "tr" {if ($$7 ~ /^{{name}}/ || $$7 == "Total") {if ($$13 ~ /%$$/) {maybe_print_headers(); print $$7": " $$13}}} $$2 == "/table" {exit} function maybe_print_headers() {if (!print_headers) {print "Coverage summary:" ; print_headers = 1}}'
COVERAGE_SUMARY_CMD          = awk $(READ_COVERAGE_SUMMARY_SCRIPT) _build/test/cover/index.html
RELEASE_DIR                  = $(CURDIR)/_build/release/rel/{{name}}


ifndef ERL
$(error Could not found Erlang/OTP ('erlc' command) installed on this system.)
endif


.PHONY: all compile shell docs test ct dialyzer cover coverage-summary release start stop remote release-shell ping clean distclean


all: test docs release

compile:
	@ $(REBAR) compile

shell: compile
ifeq (,$(wildcard $(CURDIR)/tools/user_default.beam))
	@ erlc -o $(CURDIR)/tools $(CURDIR)/tools/user_default.erl
endif
	@ erl -pa $(shell ls -d _build/default/lib/*/ebin) -pz $(CURDIR)/tools -config $(CFG_DIR)/sys.config -args_file $(CFG_DIR)/vm.args -eval "catch code:load_file('{{name}}')"

docs: compile
	@ $(REBAR) as doc edoc

test:
	@ $(REBAR) as test do dialyzer, ct, cover
	@ $(COVERAGE_SUMARY_CMD)

ct:
	@ $(REBAR) as test ct

dialyzer:
	@ $(REBAR) as test dialyzer

cover:
	@ $(REBAR) as test ct, cover
	@ $(COVERAGE_SUMARY_CMD)

coverage-summary:
	@ $(COVERAGE_SUMARY_CMD)

release:
	@ $(REBAR) as release release

start:
	@ $(RELEASE_DIR)/bin/{{name}} start

stop:
	@ $(RELEASE_DIR)/bin/{{name}} stop

remote:
	@ $(RELEASE_DIR)/bin/{{name}} remote_console

release-shell:
	@ $(RELEASE_DIR)/bin/{{name}} console

ping:
	@ $(RELEASE_DIR)/bin/{{name}} ping

clean:
	@ $(REBAR) clean

distclean: clean
	@ rm -rf _build rebar.lock doc
