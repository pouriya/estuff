.Phony: install uninstall

install:
	@ mkdir -p ~/.config/rebar3
	@ mkdir -p ~/.config/rebar3/templates
	cp ./src/* ~/.config/rebar3/templates/

uninstall:
	rm ~/.config/rebar3/templates/estuff*
