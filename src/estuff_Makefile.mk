CFG_DIR                      = $(CURDIR)/config
TOOLS_DIR                    = $(CURDIR)/tools
REBAR                        = $(TOOLS_DIR)/rebar3
ERL                         := $(shell command -v erl 2> /dev/null)
RELEASE_DIR                  = $(CURDIR)/_build/default/rel/{{name}}
VERSION                     := $(shell cat VERSION | tr -ds \n \r)
RELEASE_NAME                 = {{name}}-$(VERSION)
SAVE_COVERAGE                = COVERAGE_SUMMARY

V    = 0
PRE  = @
POST = > /dev/null

ifeq ($(V),1)
PRE  =
POST =
endif


ifndef ERL
$(error Could not found Erlang/OTP ('erlc' command) installed on this system.)
endif


.PHONY: all compile shell docs test dialyzer cover release clean distclean


all: test docs release clean


compile:
	@ echo Compiling code
	$(PRE) $(REBAR) compile $(POST)
	$(PRE) cp -r $(CURDIR)/_build/default/lib/{{name}}/ebin $(CURDIR)


shell: maybe-compile-user_default
	@ echo Compiling code
	$(PRE) $(REBAR) compile $(POST) && \
erl -pa $(shell ls -d _build/default/lib/*/ebin) \
    -pz $(TOOLS_DIR) \
    -config $(CFG_DIR)/sys.config \
    -args_file $(CFG_DIR)/vm.args \
    -eval "begin application:load({{name}}), catch code:load_file('{{name}}') end"


maybe-compile-user_default:
ifeq (,$(wildcard $(TOOLS_DIR)/user_default.beam))
	@ echo Compiling user_default module
	$(PRE) erlc -o $(TOOLS_DIR) $(TOOLS_DIR)/user_default.erl $(POST)
endif


docs:
	@ echo Building documentation
	$(PRE) $(REBAR) as doc edoc $(POST)


test: cover


dialyzer: compile
	@ echo Running dialyzer
	$(PRE) $(REBAR) as test dialyzer $(POST)


cover: compile
	@ echo Running tests
	$(PRE) $(REBAR) as test do ct, cover $(POST)
	@ echo Coverage summary:
	$(PRE) awk -f $(TOOLS_DIR)/coverage_summary.awk indent="\t" \
                  $(CURDIR)/_build/test/cover/index.html || true
	$(PRE) awk -f $(TOOLS_DIR)/coverage_summary.awk \
                  indent="" \
                  pre_name="" \
                  post_name="" \
                  pre_low_percentage="" \
                  post_low_percentage="" \
                  pre_normal_percentage="" \
                  post_normal_percentage="" \
                  pre_high_percentage="" \
                  post_high_percentage="" \
                  $(CURDIR)/_build/test/cover/index.html \
                  > $(SAVE_COVERAGE) || true


release: compile
	@ echo Building release $(RELEASE_NAME)
	$(PRE) $(REBAR) release $(POST)
	$(PRE) mkdir -p $(CURDIR)/$(RELEASE_NAME) $(POST)
	$(PRE) cp -r $(RELEASE_DIR)/* $(CURDIR)/$(RELEASE_NAME) $(POST)
	$(PRE) tar -zcvf $(RELEASE_NAME).tar.gz --absolute-names $(CURDIR)/$(RELEASE_NAME) $(POST)


clean:
	@ echo Cleaning out
	$(PRE) $(REBAR) clean $(POST)


distclean: clean
	$(PRE) rm -rf _build rebar.lock doc $(RELEASE_NAME)* ebin $(POST)
