.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W -o ../ebin $<

.beam: .erm

VPATH = ../ebin

ERL = erl -boot start_clean

MODS = utf8 evoxml evorender evo evotest evorun \
	evotemplate evopgsql

all: compile

compile: ${MODS:%=%.beam}
