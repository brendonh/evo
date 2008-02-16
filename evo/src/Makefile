.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W -o ../ebin $<

.beam: .erm

VPATH = ../ebin

ERL = erl -boot start_clean

MODS = evo evotest

all: compile

compile: ${MODS:%=%.beam}
