%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couchapp_util).

-include("couchapp.hrl").

-define(BLOCKSIZE, 32768).
-define(SEPARATOR, $\/).

-export([md5_file/1, 
         abort/2,
         get_cwd/0,
         normalize_path/1,
         in_couchapp/1,
         db_from_string/1]).


%% ====================================================================
%% Public API
%% ====================================================================
db_from_string(DbString) ->
    ok.

in_couchapp("/") ->
    {error, not_found};
in_couchapp(Path) ->
    RcPath = filename:join(Path, ".couchapprc"),
    case filelib:is_regular(RcPath) of
        true ->
            {ok, Path};
        false ->
            in_couchapp(normalize_path(filename:join(Path, "../")))
    end.

%% normalize path.
normalize_path(Path)  ->
    "/" ++ string:join(normalize_path1(string:tokens(Path,
                "/"), []), [?SEPARATOR]).

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





normalize_path1([], Acc) ->
    lists:reverse(Acc);
normalize_path1([".."|Rest], Acc) ->
    Acc1 = case Acc of
        [] -> [".."|Acc];
        [T|_] when T =:= ".." -> [".."|Acc];
        [_|R] -> R
    end,
    normalize_path1(Rest, Acc1);
normalize_path1(["."|Rest], Acc) ->
    normalize_path1(Rest, Acc);
normalize_path1([Path|Rest], Acc) ->
    normalize_path1(Rest, [Path|Acc]).

loop (P, C) ->
    case file:read(P, ?BLOCKSIZE) of
    {ok, Bin} ->
        loop(P, erlang:md5_update(C, Bin));
    eof ->
        file:close(P),
        {ok, erlang:md5_final(C)}
    end.
