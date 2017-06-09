# compile:
# 	@rm -Rf ebin
# 	@mkdir ebin
# 	@erlc -W0 -o ebin src/.erl 

run1:
	@erl -run bridge_test test  -noshell -s erlang halt

run2:
	@erl -run bridge_fair_test test  -noshell -s erlang halt

run3:
	@erl -run bridge_fair_cpre_test test  -noshell -s erlang halt

run4:
	@erl -run reader_writer_cpre_test test  -noshell -s erlang halt