
.PHONY: deps doc

all: deps compile

compile:
	@./rebar compile
	./bootstrap

deps:
	@./rebar get-deps

doc:
	@mkdir -p doc
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}, {dir, "./doc"}]'
	

clean: 
	@./rebar clean
	@rm -f couchapp

distclean: clean
	@./rebar delete-deps


