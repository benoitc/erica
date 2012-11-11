
.PHONY: deps doc

PREFIX?= /usr/local

all: deps compile

install: all
	@install -m 0755 -c erica $(PREFIX)/bin
	
compile:
	@./rebar compile
	@escript bootstrap

deps:
	@./rebar get-deps

doc:
	@mkdir -p doc
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}, {dir, "./doc"}]'


clean:
	@./rebar clean
	@rm -f erica

distclean: clean
	@./rebar delete-deps


