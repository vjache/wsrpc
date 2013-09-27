REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) get-deps

compile: deps
	@$(REBAR) compile

test: force
	@$(REBAR) eunit

test_js:
	env NODE_PATH=priv/node_modules mocha

run: clean compile
	./start.sh

clean:
	@$(REBAR) clean
	rm -rf log/
	rm -rf logs/
