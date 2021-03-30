.PHONY: escript

escript:
	./rebar3 escriptize
	cp _build/default/bin/docwatch ./
