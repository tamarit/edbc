compile:
	@rm -Rf deps/parse_trans/ebin
	@mkdir deps/parse_trans/ebin
	@erlc -W0 -o deps/parse_trans/ebin deps/parse_trans/src/*.erl 
	@rm -Rf deps/sheriff/ebin
	@mkdir deps/sheriff/ebin
	@erlc -W0 -pa deps/parse_trans/ebin -o deps/sheriff/ebin deps/sheriff/src/*.erl
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -pa deps/sheriff/ebin -o ebin src/*.erl  

# Test cases

run_bridge_fair:
	@cd examples/bridge/fair;\
	rm -Rf ebin;\
	mkdir ebin;\
	erlc -pa ../../../ebin -pa ../../../deps/sheriff/ebin -pa ../../../deps/parse_trans/ebin -I ../../../include -W0 -o ebin -Dedbc src/*.erl;\
	erl -pa ebin ../../../ebin ../../../deps/sheriff/ebin ../../../deps/parse_trans/ebin -run bridge_test test  -noshell  -eval -s erlang halt

run_bridge_unfair:
	@cd examples/bridge/unfair;\
	rm -Rf ebin;\
	mkdir ebin;\
	erlc -pa ../../../ebin -pa ../../../deps/sheriff/ebin -pa ../../../deps/parse_trans/ebin -I ../../../include -W0 -o ebin -Dedbc src/*.erl;\
	erl -pa ebin ../../../ebin ../../../deps/sheriff/ebin ../../../deps/parse_trans/ebin -run bridge_test test  -noshell  -eval -s erlang halt

load_ej1:
	@cd examples/other;\
	rm -Rf ebin;\
	mkdir ebin;\
	erlc -pa ../../ebin -pa ../../deps/sheriff/ebin -pa ../../deps/parse_trans/ebin -I ../../include -W0 -o ebin -Dedbc src/ej1.erl;\
	erl -pa ebin ../../ebin ../../deps/sheriff/ebin ../../deps/parse_trans/ebin

load_ej1_noedbc:
	@cd examples/other;\
	rm -Rf ebin;\
	mkdir ebin;\
	erlc -pa ../../ebin -pa ../../deps/sheriff/ebin -pa ../../deps/parse_trans/ebin -I ../../include -W0 -o ebin src/ej1.erl;\
	erl -pa ebin ../../ebin ../../deps/sheriff/ebin ../../deps/parse_trans/ebin

doc_ej1:
	@cd examples/other;\
	rm -Rf ebin;\
	mkdir ebin;\
	erlc -pa ../../ebin -pa ../../deps/sheriff/ebin -pa ../../deps/parse_trans/ebin -I ../../include -W0 -o ebin src/ej1.erl;\
	cd ../../;\
	erl -pa ebin deps/sheriff/ebin deps/parse_trans/ebin -eval 'edbc_parse_transform:print_clean_code("examples/other/src/ej1.erl", ["include"], "examples/other/ej1.erl")' -noshell  -eval -s erlang halt;\
	erl -run edoc_run files '["examples/other/ej1.erl"]' '[{dir, "examples/other/docs"}]' -noshell -eval -s erlang halt;\
	rm -rf examples/other/ej1.erl

run_library:
	@cd examples/other;\
	rm -Rf ebin;\
	mkdir ebin;\
	erlc -pa ../../ebin -pa ../../deps/sheriff/ebin -pa ../../deps/parse_trans/ebin -I ../../include -W0 -o ebin -Dedbc src/library*.erl;\
	erl -pa ebin ../../ebin ../../deps/sheriff/ebin ../../deps/parse_trans/ebin -run library_test test1  -noshell  -eval -s erlang halt

run_rw_unfair4writers:
	@cd examples/readers_writers/unfair4writers;\
	rm -Rf ebin;\
	mkdir ebin;\
	erlc -pa ../../../ebin -pa ../../../deps/sheriff/ebin -pa ../../../deps/parse_trans/ebin -I ../../../include -W0 -o ebin -Dedbc src/*.erl;\
	erl -pa ebin ../../../ebin ../../../deps/sheriff/ebin ../../../deps/parse_trans/ebin -run readers_writers_test test  -noshell  -eval -s erlang halt

run_rw_unfair4readers:
	@cd examples/readers_writers/unfair4readers;\
	rm -Rf ebin;\
	mkdir ebin;\
	erlc -pa ../../../ebin -pa ../../../deps/sheriff/ebin -pa ../../../deps/parse_trans/ebin -I ../../../include -W0 -o ebin -Dedbc src/*.erl;\
	erl -pa ebin ../../../ebin ../../../deps/sheriff/ebin ../../../deps/parse_trans/ebin -run readers_writers_test test  -noshell  -eval -s erlang halt

run_rw_fair:
	@cd examples/readers_writers/fair;\
	rm -Rf ebin;\
	mkdir ebin;\
	erlc -pa ../../../ebin -pa ../../../deps/sheriff/ebin -pa ../../../deps/parse_trans/ebin -I ../../../include -W0 -o ebin -Dedbc src/*.erl;\
	erl -pa ebin ../../../ebin ../../../deps/sheriff/ebin ../../../deps/parse_trans/ebin -run readers_writers_test test  -noshell  -eval -s erlang halt