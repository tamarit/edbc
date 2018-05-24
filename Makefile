compile:
	@make depend
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -pa deps/sheriff/ebin -o ebin src/*.erl  

depend:
	@rm -Rf deps/parse_trans/ebin
	@mkdir deps/parse_trans/ebin
	@erlc -W0 -o deps/parse_trans/ebin deps/parse_trans/src/*.erl 
	@rm -Rf deps/sheriff/ebin
	@mkdir deps/sheriff/ebin
	@erlc -W0 -pa deps/parse_trans/ebin -o deps/sheriff/ebin deps/sheriff/src/*.erl

# Test cases

run_bridge_fair:
	@scripts/edbc_erlc "examples/bridge/fair/src/*.erl" examples/bridge/fair/ebin
	@scripts/edbc_erl examples/bridge/fair/ebin "bridge_test:test()"

run_bridge_unfair:
	@scripts/edbc_erlc "examples/bridge/unfair/src/*.erl" examples/bridge/unfair/ebin
	@scripts/edbc_erl examples/bridge/unfair/ebin "bridge_test:test()"

load_ej1:
	@scripts/edbc_erlc examples/other/src/ej1.erl examples/other/ebin
	@scripts/edbc_erl examples/other/ebin 

load_ej1_noedbc:
	@scripts/edbc_erlcp examples/other/src/ej1.erl examples/other/ebin
	@scripts/edbc_erl examples/other/ebin 

doc_ej1:
	@scripts/edbc_edoc examples/other/src/ej1.erl examples/other/docs

load_merge:
	@scripts/edbc_erlc examples/other/src/merge.erl examples/other/ebin
	@scripts/edbc_erl examples/other/ebin 

run_library:
	@scripts/edbc_erlc "examples/other/src/library*.erl" examples/other/ebin
	@scripts/edbc_erl examples/other/ebin "library_test:test1()"

run_rw_unfair4writers:
	@scripts/edbc_erlc "examples/readers_writers/unfair4writers/src/*.erl" examples/readers_writers/unfair4writers/ebin
	@scripts/edbc_erl examples/readers_writers/unfair4writers/ebin "readers_writers_test:test()"

run_rw_unfair4readers:
	@scripts/edbc_erlc "examples/readers_writers/unfair4readers/src/*.erl" examples/readers_writers/unfair4readers/ebin
	@scripts/edbc_erl examples/readers_writers/unfair4readers/ebin "readers_writers_test:test()"

run_rw_fair:
	@scripts/edbc_erlc "examples/readers_writers/fair/src/*.erl" examples/readers_writers/fair/ebin
	@scripts/edbc_erl examples/readers_writers/fair/ebin "readers_writers_test:test()"

test_semaphore:
	@scripts/edbc_erlc "examples/semaphore/no_queues/src/*.erl" examples/semaphore/no_queues/ebin
	@scripts/edbc_erl examples/semaphore/no_queues/ebin "semaphore_tests:test()"

test_sel_recv:
	@scripts/edbc_erlc "examples/sel_recv/gen_server/src/*.erl" examples/sel_recv/gen_server/ebin
	@erl -pa  examples/sel_recv/gen_server/ebin -eval "sel_recv_test:test()" -eval -s erlang halt  

test_sel_recv_q:
	@scripts/edbc_erlc "examples/sel_recv/gen_server_qcpre/src/*.erl" examples/sel_recv/gen_server_qcpre/ebin
	@erl -pa "examples/sel_recv/gen_server_qcpre/ebin" "ebin" "deps/sheriff/ebin" "deps/parse_trans/ebin" -eval "sel_recv_test:test()" -s erlang halt  

load_ej_paper:
	@scripts/edbc_erlc examples/other/src/ej_paper.erl examples/other/ebin
	@scripts/edbc_erl examples/other/ebin 

load_ej_temp:
	@scripts/edbc_erlc examples/other/src/ej_temp.erl examples/other/ebin
	@scripts/edbc_erl examples/other/ebin 

# Test cases (Queued versions)

run_rw_fair_q:
	@scripts/edbc_erlc "examples/readers_writers/fair_queues/src/*.erl" examples/readers_writers/fair_queues/ebin
	@scripts/edbc_erl examples/readers_writers/fair_queues/ebin "readers_writers_test:test()"

run_rw_unfair4writers_q:
	@scripts/edbc_erlc "examples/readers_writers/unfair4writers_queues/src/*.erl" examples/readers_writers/unfair4writers_queues/ebin
	@scripts/edbc_erl examples/readers_writers/unfair4writers_queues/ebin "readers_writers_test:test()"

run_rw_unfair4readers_q:
	@scripts/edbc_erlc "examples/readers_writers/unfair4readers_queues/src/*.erl" examples/readers_writers/unfair4readers_queues/ebin
	@scripts/edbc_erl examples/readers_writers/unfair4readers_queues/ebin "readers_writers_test:test()"

test_semaphore_q:
	@scripts/edbc_erlc "examples/semaphore/queues/src/*.erl" examples/semaphore/queues/ebin
	@scripts/edbc_erl examples/semaphore/queues/ebin "semaphore_tests:test()"
