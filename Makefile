.PHONY: all deps compile clean test release

REBAR=$(shell which rebar3 || echo ./rebar3)
REBAR_URL=https://s3.amazonaws.com/rebar3/rebar3

all: clean compile

deps: $(REBAR)
	$(REBAR) get-deps

compile: $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean
	rm -rf ./erl_crash.dump

test: $(REBAR)
	$(REBAR) eunit

# Get rebar3 if it doesn't exist. If rebar3 was found on PATH, the
# $(REBAR) dep will be satisfied since the file will exist.
./rebar3:
	@echo "Fetching rebar3 from $(REBAR_URL)"
	@erl -noinput -noshell -s inets -s ssl  -eval '{ok, _} = httpc:request(get, {"${REBAR_URL}", []}, [], [{stream, "${REBAR}"}])' -s init stop
		chmod +x ${REBAR}

