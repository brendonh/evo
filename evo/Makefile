PKG_NAME = evo

SRC_DIR = src
EBIN_DIR = ebin
INCLUDE_DIR = include

MOCHIWEB_EBIN = ../../mochiweb/ebin
COMETD_EBIN = ../../cometd/ebin
COUCHDB_EBIN = ../../../lib/erlang_couchdb/ebin
OPENID_EBIN = ../../erl_openid/ebin

SOURCES  = $(wildcard $(SRC_DIR)/*.erl)
INCLUDES = $(wildcard $(INCLUDE_DIR)/*.hrl)
TARGETS  = $(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

RABBITMQ_INCLUDE = /usr/local/share/rabbitmq/include
OPENID_INCLUDE = ../../erl_openid/src

ERLC_INCLUDES = -I $(INCLUDE_DIR) -I $(RABBITMQ_INCLUDE) -I $(OPENID_INCLUDE)
ERL_EBINS = -pa $(EBIN_DIR) \
            -pa $(MOCHIWEB_EBIN) -pa $(COMETD_EBIN) -pa $(COUCHDB_EBIN) -pa $(OPENID_EBIN)

ERLC = erlc
ERLC_OPTS = $(ERLC_INCLUDES) -o $(EBIN_DIR) -Wall -v +debug_info

ERL_CMD=erl \
	-boot start_sasl \
	-config $(PKG_NAME) \
	+W w \
	$(ERL_EBINS)

all: $(TARGETS)

run_prereqs: all

run: run_prereqs
	$(ERL_CMD) -s ${PKG_NAME}_app launch

stop:
	erl_call -a '$(PKG_NAME)_app stop_and_halt []' -sname $(PKG_NAME)

clean: cleanlog
	rm -f $(TARGETS)
	rm -f $(EBIN_DIR)/*.beam

cleanlog:
	rm -f auth.log report.log sasl_err.log
	rm -f *.access

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_OPTS) $<
