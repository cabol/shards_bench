REBAR = $(shell which rebar3 || echo ./rebar3)

BUILD_PATH = ./_build/default/lib/*/ebin

CONFIG ?= config/sys.config

.PHONY: all compile clean distclean run

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) clean --all
	rm -rf _build *.lock log

run: all
	mkdir -p log
	erl +P 10000000 -pa $(BUILD_PATH) -s shards_bench -config ${CONFIG}

run_ets: all
	mkdir -p log
	erl +P 10000000 -pa $(BUILD_PATH) -s shards_bench -config config/ets.config

run_shards: all
	mkdir -p log
	erl +P 10000000 -pa $(BUILD_PATH) -s shards_bench -config config/shards.config

run_mnesia: all
	mkdir -p log
	erl +P 10000000 -pa $(BUILD_PATH) -s shards_bench -config config/mnesia.config

clean_log: clean
	rm -rf log
