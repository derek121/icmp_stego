all: compile

compile:
	rm -f deps/procket/priv/procket
	rebar compile

deps:
	rebar get-deps

chmod:
	sudo chown root deps/procket/priv/procket
	sudo chmod 4750 deps/procket/priv/procket

run:
	erl -pa ebin -pa deps/*/ebin -config config/sys -s icmp_data

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@rebar skip_deps=true eunit


clean:
	rebar clean

