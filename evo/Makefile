.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W -o ebin $<

.beam: .erl

VPATH = ebin

ERL = erl -boot start_clean

MODS = 

all: compile

.PHONY: clean cleaner run

run: compile
	./go

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
