.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W -o ebin $<

.beam: .erl

VPATH = ebin

ERL = erl -boot start_clean

MODS = 

all: compile

.PHONY: clean cleaner run

test: compile
	erl -noshell -pa ebin -pa ../avl_common/ebin -s evotest test -s init stop

run: compile
	erl -noshell -pa ebin -s evorun run -s init stop

site: compile
	erl -boot start_sasl \
		-pa ebin \
		-pa ../mochiweb/ebin \
		-pa ../RouteComplete/avl-common/ebin \
		-config evo.config \
		-s evo_entry start

pgsql: compile
	erl -pa ebin \
		-pa ../mochiweb/ebin \
		-pa ../RouteComplete/avl-common/ebin \
		-s evopgsql start 8081

pgsql_sasl: compile
	erl -boot start_sasl \
		-pa ebin \
		-pa ../mochiweb/ebin \
		-pa ../RouteComplete/avl-common/ebin \
		-s evopgsql start 8081

compile: ${MODS:%=%.beam} subdirs

subdirs:
	cd src; make

clean:
	rm -f erl_crash.dump
	cd ebin; rm -f *

cleaner: clean
	rm -f err.log
	rm -f logs/*
	find . -name '*~' | xargs rm
