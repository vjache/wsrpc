REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test: force
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
