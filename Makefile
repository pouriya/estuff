.Phony: install uninstall

install:
	@ mkdir -p ~/.config/rebar3
	@ mkdir -p ~/.config/rebar3/templates
	install ./src/* ~/.config/rebar3/templates/

uninstall:
	rm -rf ~/.config/rebar3/templates/estuff*
