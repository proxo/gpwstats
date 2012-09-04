REBAR=./rebar
all: deps compile
app:
	@$(REBAR) compile skip_deps=true
compile:
	@$(REBAR) compile
deps: 
	@$(REBAR) get-deps	
clean:
	@$(REBAR) clean
test: app
	@$(REBAR) eunit skip_deps=true
start:
	exec erl -pa $(PWD)/ebin  -pa $(PWD)/deps/*/ebin -config $(PWD)/priv/app.config -s gpwstats
