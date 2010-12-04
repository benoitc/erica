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
         v2a/1,
         relpath/2,
         parse_couchapp_url/1,
         make_dir/1]).


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

    "/" ++ DbName = Url#url.path,
    {ok, Db} = couchbeam:open_or_create_db(Server, DbName, Options),
    Db.


db_from_config(Config, DbString) ->
    case mochiweb_util:urlsplit(DbString) of 
        {[], [], _Path, _, _} ->
            case couchapp_config:get_db(Config, DbString) of
                undefined ->
                    db_from_string(DbString);
                Db ->
                    Db
            end;
        _ ->
            db_from_string(DbString)
    end.

parse_couchapp_url(AppUrl) ->
    Url = ibrowse_lib:parse_url(AppUrl),
    PathParts = string:tokens(Url#url.path, "/"),

    case parse_couchapp_path(PathParts) of
        {DbName, AppName, DocId} ->
            Server = couchbeam:server_connection(Url#url.host, 
                Url#url.port),
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
            {ok, Db} = couchbeam:open_or_create_db(Server, DbName, 
                Options),
            {ok, Db, AppName, DocId};
        Error ->
            Error
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


make_dir(Dir) ->
    ok = filelib:ensure_dir(Dir),

    ok = case filelib:is_dir(Dir) of
        true -> ok;
        false ->
            file:make_dir(Dir)
    end.

-spec abort(string(), [term()]) -> no_return().
abort(String, Args) -> 
    ?ERROR(String, Args),
    halt(1).

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

relpath(Path, Root) ->
    {_, _, RelPath} = mochiweb_util:partition(Path, Root),
    case string:left(RelPath, 1) of
        " " ->
            "";
        "/" -> 
            "/" ++ RelPath1 = RelPath,
            RelPath1;
        "\\" ->
            "\\\\" ++ RelPath1 = RelPath,
            RelPath1
    end.

%% function from pragmatic programmer book.
md5_file(File) ->
    case file:open(File, [binary,raw,read]) of
    {ok, P} -> loop(P, crypto:md5_init());
    Error   -> Error
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================


parse_couchapp_path([DbName, "_design", AppName|_]) ->
    {DbName, AppName, "_design/" ++ AppName};
parse_couchapp_path([DbName, DocId]) ->
    {DbName, DocId, DocId};
parse_couchapp_path(_) ->
    invalid_couchapp_url.

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
        loop(P, crypto:md5_update(C, Bin));
    eof ->
        file:close(P),
        {ok, crypto:md5_final(C)}
    end.
