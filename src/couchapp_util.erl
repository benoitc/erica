%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couchapp_util).

-include("couchapp.hrl").

-define(BLOCKSIZE, 32768).

-export([md5_file/1, 
         abort/2,
         get_cwd/0]).


%% ====================================================================
%% Public API
%% ====================================================================
-spec abort(string(), [term()]) -> no_return().
abort(String, Args) -> 
    ?ERROR(String, Args),
    halt(1).

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.


%% function from pragmatic programmer book.
md5_file(File) ->
    case file:open(File, [binary,raw,read]) of
    {ok, P} -> loop(P, erlang:md5_init());
    Error   -> Error
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================


loop (P, C) ->
    case file:read(P, ?BLOCKSIZE) of
    {ok, Bin} ->
        loop(P, erlang:md5_update(C, Bin));
    eof ->
        file:close(P),
        {ok, erlang:md5_final(C)}
    end.
