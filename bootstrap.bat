@echo off
call rebar get-deps
echo %%%% >> deps/proper/include/compile_flags.hrl
call rebar compile
