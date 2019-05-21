# {{header_comment}}

CFG_DIR      = $(CURDIR)/config
TOOLS_DIR    = $(CURDIR)/tools
REBAR        = $(TOOLS_DIR)/rebar3
ERL         := $(shell command -v erl 2> /dev/null)
RELEASE_DIR  = $(CURDIR)/_build/default/rel/{{name}}
VERSION     := $(shell erl -noshell -eval 'io:format("~s", [case file:consult("$(CURDIR)/src/{{name}}.app.src") of {ok, [{_, _, Terms}]} -> proplists:get_value(vsn, Terms, "0.0.0"); _ -> "0.0.0" end]),erlang:halt().')
RELEASE_NAME = {{name}}-$(VERSION)
NAME_UPPER  := $(shell echo {{name}} | awk '{print toupper($$1)}')

PRE         = @
POST        =
BUILD_DEBUG = 0
REBAR_DEBUG =

v = 1
ifeq ($(v),0)
POST = > /dev/null
endif

ifeq ($(v),2)
PRE =
endif

ifeq ($(v),3)
PRE         =
BUILD_DEBUG = 1
endif

ifeq ($(v),4)
PRE         =
BUILD_DEBUG = 1
REBAR_DEBUG = 1
endif

coverage = 0
ifeq ($(coverage),0)
coverage = /dev/null
endif



ifndef ERL
$(error Could not found Erlang/OTP ('erl' command) installed on this system.)
endif


.PHONY: all compile shell docs test dialyzer cover release package package-src package-app package-release clean distclean docker push


all: test docs package


compile:
	@ echo Compiling code
	$(PRE) \
            export $(NAME_UPPER)_BUILD=COMPILE && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            export $(NAME_UPPER)_BUILD_DEBUG=$(BUILD_DEBUG) && \
            $(REBAR) compile \
        $(POST)
	$(PRE) cp -r $(CURDIR)/_build/default/lib/{{name}}/ebin $(CURDIR)


shell:
	@ echo Compiling user_default module
	$(PRE) erlc -o $(TOOLS_DIR) $(TOOLS_DIR)/user_default.erl $(POST)
	$(PRE) \
            export $(NAME_UPPER)_BUILD=SHELL && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            export $(NAME_UPPER)_BUILD_DEBUG=$(BUILD_DEBUG) && \
            $(REBAR) compile \
        $(POST) && \
        erl     -pa `ls -d _build/default/lib/*/ebin` \
                -pz $(TOOLS_DIR) \
                -config $(CFG_DIR)/sys.config \
                -args_file $(CFG_DIR)/vm.args \
                -eval "begin application:load('{{name}}'), catch code:load_file('{{name}}') end" \
                +B


docs:
	@ echo Building documentation
	$(PRE) \
            export $(NAME_UPPER)_BUILD=DOC && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            export $(NAME_UPPER)_BUILD_DEBUG=$(BUILD_DEBUG) && \
            $(REBAR) edoc \
        $(POST)


test: cover dialyzer


dialyzer: compile
	@ echo Running dialyzer
	$(PRE) \
            export $(NAME_UPPER)_BUILD=DIALYZER && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            export $(NAME_UPPER)_BUILD_DEBUG=$(BUILD_DEBUG) && \
            $(REBAR) dialyzer \
        $(POST)


cover: compile
	@ echo Running tests
	$(PRE) \
            export $(NAME_UPPER)_BUILD=TEST && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            export $(NAME_UPPER)_BUILD_DEBUG=$(BUILD_DEBUG) && \
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
            export $(NAME_UPPER)_BUILD_DEBUG=$(BUILD_DEBUG) && \
            $(REBAR) release \
        $(POST)
	$(PRE) mkdir -p $(CURDIR)/$(RELEASE_NAME) $(POST)
	$(PRE) cp -r $(RELEASE_DIR)/* $(CURDIR)/$(RELEASE_NAME) $(POST)


package-release: release
	@ echo Packaging release to $(RELEASE_NAME)-release.tar.gz
	$(PRE) \
            rm -rf .tar && \
            find $(RELEASE_NAME)/ -type f > .tar && \
            tar -zcvf $(RELEASE_NAME)-release.tar.gz -T .tar $(POST) && \
            rm -rf .tar $(POST)

package-src: compile
	@ echo Packaging source to $(RELEASE_NAME)-src.tar.gz
	$(PRE) \
            rm -rf .tar && \
            find src/ -type f > .tar && \
            find include/ -type f >> .tar || true && \
            find config/ -type f >> .tar && \
            find tools/ -type f >> .tar && \
            find test/ -type f >> .tar && \
            find priv/ -type f >> .tar || true && \
            echo Dockerfile >> .tar || true && \
            echo LICENSE >> .tar && \
            echo Makefile >> .tar && \
            echo README.md >> .tar && \
            echo rebar.config >> .tar || true && \
            echo rebar.config.script >> .tar && \
            tar -zcvf $(RELEASE_NAME)-src.tar.gz -T .tar $(POST) && \
            rm -rf .tar $(POST)


package-app: compile
	@ echo Packaging application to $(RELEASE_NAME)-app.tar.gz
	$(PRE) \
            rm -rf .tar && \
            find src/ -type f > .tar && \
            find include/ -type f >> .tar || true && \
            find ebin/ -type f >> .tar && \
            find priv/ -type f >> .tar || true && \
            tar -zcvf $(RELEASE_NAME)-app.tar.gz -T .tar $(POST) && \
            rm -rf .tar $(POST)


package: package-src package-app package-release


clean:
	@ echo Cleaning out
	$(PRE) \
            export $(NAME_UPPER)_BUILD=CLEAN && \
            export DEBUG=$(REBAR_DEBUG) && \
            export $(NAME_UPPER)_VERSION=$(VERSION) && \
            export $(NAME_UPPER)_BUILD_DEBUG=$(BUILD_DEBUG) && \
            $(REBAR) clean $(POST)
	$(PRE) rm -rf $(CURDIR)/ebin $(POST)


distclean: clean
	$(PRE) rm -rf _build rebar.lock $(RELEASE_NAME) $(RELEASE_NAME)-*.tar.gz ebin tools/user_default.beam *.crashdump $(POST)


docker:
	$(PRE) docker build -t {{name}} ./ $(POST)


push: cover
	@ echo Pushing to master branch
	$(PRE) git push origin master
