compile:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -o ebin src/*.erl 

# Test cases

run_bridge:
	@cd examples/bridge/good_cpre;rm -Rf ebin; mkdir ebin; erlc -pa ../../../ebin -I ../../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../../ebin -run bridge_good_cpre_test test  -noshell  -eval -s erlang halt

load_ej1:
	@cd examples/other;rm -Rf ebin; mkdir ebin; erlc -pa ../../ebin -I ../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../ebin

run_library:
	@cd examples/other;rm -Rf ebin; mkdir ebin; erlc -pa ../../ebin -I ../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../ebin -run library_test test1  -noshell  -eval -s erlang halt

run_rw:
	@cd examples/reader_writer;rm -Rf ebin; mkdir ebin; erlc -pa ../../ebin -I ../../lib -W0 -o ebin -Dedbc src/*.erl; erl -pa ebin ../../ebin -run reader_writer_cpre_test test  -noshell  -eval -s erlang halt
