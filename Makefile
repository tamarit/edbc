compile:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -o ebin src/*.erl 

# Test cases

run_bridge_fair:
	@cd examples/bridge/fair;rm -Rf ebin; mkdir ebin; erlc -pa ../../../ebin -I ../../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../../ebin -run bridge_test test  -noshell  -eval -s erlang halt

run_bridge_unfair:
	@cd examples/bridge/unfair;rm -Rf ebin; mkdir ebin; erlc -pa ../../../ebin -I ../../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../../ebin -run bridge_test test  -noshell  -eval -s erlang halt

load_ej1:
	@cd examples/other;rm -Rf ebin; mkdir ebin; erlc -pa ../../ebin -I ../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../ebin

run_library:
	@cd examples/other;rm -Rf ebin; mkdir ebin; erlc -pa ../../ebin -I ../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../ebin -run library_test test1  -noshell  -eval -s erlang halt

run_rw_unfair4writers:
	@cd examples/readers_writers/unfair4writers;rm -Rf ebin; mkdir ebin; erlc -pa ../../../ebin -I ../../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../../ebin -run readers_writers_test test  -noshell  -eval -s erlang halt

run_rw_unfair4readers:
	@cd examples/readers_writers/unfair4readers;rm -Rf ebin; mkdir ebin; erlc -pa ../../../ebin -I ../../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../../ebin -run readers_writers_test test  -noshell  -eval -s erlang halt

run_rw_fair:
	@cd examples/readers_writers/fair;rm -Rf ebin; mkdir ebin; erlc -pa ../../../ebin -I ../../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../../ebin -run readers_writers_test test  -noshell  -eval -s erlang halt