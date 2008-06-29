.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W -o ../ebin $<

.beam: .erm

VPATH = ../ebin

ERL = erl -boot start_clean

MODS = utf8 evoxml evorender evo evotest evorun \
	evotemplate evopgsql \
    evo_entry evo_app evo_sup evosite_sup evosite \
	evoutil evostatic evotable evoscratch evoform


all: compile

compile: ${MODS:%=%.beam}
