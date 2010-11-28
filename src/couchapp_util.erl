%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couchapp_util).

-include("deps/ibrowse/src/ibrowse.hrl").
-include("couchapp.hrl").

-define(BLOCKSIZE, 32768).
-define(SEPARATOR, $\/).

-export([md5_file/1, 
         abort/2,
         get_cwd/0,
         find_executable/1,
         normalize_path/1,
         user_path/0,
         in_couchapp/1,
         db_from_string/1,
         db_from_config/2,
         v2a/1]).


%% ====================================================================
%% Public API
%% ====================================================================

v2a(V) when is_atom(V) ->
    V;
v2a(V) when is_list(V) ->
    list_to_atom(V);
v2a(V) when is_binary(V) ->
    list_to_atom(binary_to_list(V)).

db_from_string(DbString) ->
    DbUrl = case mochiweb_util:urlsplit(DbString) of 
        {[], [], Path, _, _} ->
            "http://127.0.0.1:5984/" ++ Path;
        _ ->
            DbString
    end,
    Url = ibrowse_lib:parse_url(DbUrl),
    Server = couchbeam:server_connection(Url#url.host, Url#url.port),

    Options = case Url#url.username of
        undefined -> [];
        Username ->
            case Url#url.password of
                undefined ->
                    [{basic_auth, {Username, ""}}];
                Password ->
                    [{basic_auth, {Username, Password}}]
            end
    end,
    couchbeam:open_or_create_db(Server, Url#url.path, Options).


db_from_config(DbString, Config) ->
    case mochiweb_util:urlsplit(DbString) of 
        {[], [], _Path, _, _} ->
            couchapp_config:get_db(Config, DbString);
        _ ->
            db_from_string(DbString)
    end.

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

user_path() ->
    case init:get_argument(home) of
        {ok, [[Home]]} ->
            Home;
        _ ->
            ""
    end.

find_executable(Name) ->
    case os:find_executable(Name) of
        false -> false;
        Path ->
            "\"" ++ filename:nativename(Path) ++ "\""
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
