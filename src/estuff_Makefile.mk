# {{header_comment}}

CFG_DIR      = $(CURDIR)/config
TOOLS_DIR    = $(CURDIR)/tools
REBAR        = $(TOOLS_DIR)/rebar3
ERL         := $(shell command -v erl 2> /dev/null)
RELEASE_DIR  = $(CURDIR)/_build/default/rel/{{name}}
VERSION     := $(shell cat VERSION | tr -ds \n \r)
RELEASE_NAME = {{name}}-$(VERSION)
NAME_UPPER  := $(shell echo {{name}} | awk '{print toupper($$1)}')

PRE         = @
POST        =
REBAR_DEBUG =

v = 1
ifeq ($(v),0)
POST = > /dev/null
endif

ifeq ($(v),2)
PRE         =
REBAR_DEBUG = 1
endif

coverage = 0
ifeq ($(coverage),0)
coverage = /dev/null
endif



ifndef ERL
$(error Could not found Erlang/OTP ('erl' command) installed on this system.)
endif


.PHONY: all compile shell docs test dialyzer cover release package tar clean distclean docker push


all: test docs package


compile:
	@ echo Compiling code
	$(PRE)                                         \
            export $(NAME_UPPER)_BUILD=COMPILE      && \
            export DEBUG=$(REBAR_DEBUG)             && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            $(REBAR) compile                           \
        $(POST)
	$(PRE) cp -r $(CURDIR)/_build/default/lib/{{name}}/ebin $(CURDIR)


shell:
	@ echo Compiling user_default module
	$(PRE) erlc -o $(TOOLS_DIR) $(TOOLS_DIR)/user_default.erl $(POST)
	$(PRE) \
            export $(NAME_UPPER)_BUILD=SHELL && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            $(REBAR) compile \
        $(POST) && \
        erl -pa `ls -d _build/default/lib/*/ebin` \
                -pz $(TOOLS_DIR) \
                -config $(CFG_DIR)/sys.config \
                -args_file $(CFG_DIR)/vm.args \
                -eval "begin application:load('{{name}}'), catch code:load_file('{{name}}') end"


docs:
	@ echo Building documentation
	$(PRE) \
            export $(NAME_UPPER)_BUILD=DOC && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            $(REBAR) edoc \
        $(POST)


test: cover


dialyzer: compile
	@ echo Running dialyzer
	$(PRE) \
            export $(NAME_UPPER)_BUILD=DIALYZER && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            $(REBAR) dialyzer \
        $(POST)


cover: compile
	@ echo Running tests
	$(PRE) \
            export $(NAME_UPPER)_BUILD=TEST && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            $(REBAR) do ct, cover \
        $(POST)
	@ echo Coverage summary:
	$(PRE) \
            awk -f $(TOOLS_DIR)/coverage_summary.awk \
                -v indent="\t" \
                -v colorize=1 \
                $(CURDIR)/_build/test/cover/index.html \
            || true
	$(PRE) \
            awk -f $(TOOLS_DIR)/coverage_summary.awk \
                   $(CURDIR)/_build/test/cover/index.html \
            > $(coverage) || true


release: compile
	@ echo Building release $(RELEASE_NAME)
	$(PRE) \
            export $(NAME_UPPER)_BUILD=RELEASE && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            $(REBAR) release \
        $(POST)
	$(PRE) mkdir -p $(CURDIR)/$(RELEASE_NAME) $(POST)
	$(PRE) cp -r $(RELEASE_DIR)/* $(CURDIR)/$(RELEASE_NAME) $(POST)


package: release
	$(PRE) tar -zcvf $(RELEASE_NAME).tar.gz $(CURDIR)/$(RELEASE_NAME) $(POST)


tar:
	$(PRE) (rm -rf ./{{name}}.tar.gz) && (find ./ -type f > ../.{{name}}_archive) && (tar -zcvf {{name}}.tar.gz -T - < ../.{{name}}_archive) && rm -rf ../.{{name}}_archive $(POST)


clean:
	@ echo Cleaning out
	$(PRE) $(REBAR) clean $(POST)
	$(PRE) rm -rf $(CURDIR)/ebin $(POST)


distclean: clean
	$(PRE) rm -rf _build rebar.lock doc $(RELEASE_NAME)* ebin $(POST)


docker:
	$(PRE) docker build -t {{name}} ./ $(POST)


push: cover
	@ echo Pushing to master branch
	$(PRE) git push origin master
