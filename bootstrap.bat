@echo off
call rebar get-deps
call rebar -D WITH_MOCHIJSON compile
