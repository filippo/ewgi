VSN		:= 0.2
ERL		?= erl
EBIN_DIRS	:= $(wildcard lib/*/ebin)
APP		:= ewgi

all: erl ebin/$(APP).app

erl: ebin lib
	@./support/compile.erl ebin src/$(APP).app $(EBIN_DIRS)

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

ebin/$(APP).app: src/$(APP).app Makefile
	@./support/gen_app_file.erl src/$(APP).app $@ $(VSN)

ebin:
	@mkdir ebin

lib:
	@mkdir lib

dialyzer: erl
	@dialyzer -c ebin

test: erl
	@$(ERL) -pa $(EBIN_DIRS) -pa ebin -noinput +B \
	-eval 'case lists:member(error, ewgi_test:test()) of true -> halt(1); _ -> halt(0) end.'
