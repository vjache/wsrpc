REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test: force
	@$(REBAR) eunit

test_js:
	env NODE_PATH=priv/node_modules mocha

clean:
	@$(REBAR) clean
