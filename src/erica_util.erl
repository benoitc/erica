%%% -*- erlang -*-
%%%
%%% This file is part of erica released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erica_util).

-include("ibrowse.hrl").
-include("erica.hrl").

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
         db_from_key/2,
         v2a/1,
         relpath/2,
         parse_couchapp_url/1,
         make_dir/1,
         escript_foldl/3,
         find_files/2]).


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
    DbUrl = case couchbeam_util:urlsplit(DbString) of
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

%% @doc fetch a couchbeam database handler by being given a url or
%% a key to lookup the url from the config
db_from_key(Config, Key) ->
    case erica_config:get_db(Config, list_to_binary(Key)) of
        undefined ->
            db_from_string(Key);
        Doc ->
            Url = couchbeam_doc:get_value(<<"db">>, Doc),
            db_from_string(binary_to_list(Url))
    end.


db_from_config(Config, DbString) ->
    case couchbeam_util:urlsplit(DbString) of
        {[], [], _Path, _, _} ->
            case erica_config:get_db(Config, DbString) of
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

    Server = couchbeam:server_connection(Url#url.host,
        Url#url.port),
    DbOptions = db_options_from_url(Url),

    case parse_couchapp_path(PathParts) of
        {DbName, AppName, DocId} ->
            {ok, Db} = couchbeam:open_or_create_db(Server, DbName,
                DbOptions),
            AppName1 = erica_config:get_global(appid, AppName),
            {ok, Db, AppName1, DocId};
        invalid_url ->
            %% did we set dbname & other things in args?
            case erica_config:get_global(db) of
                undefined ->
                    {error, 
                        "clone: invalid url and db name not provided"};
                DbName ->
                    case erica_config:get_global(appid) of
                        undefined ->
                            {error, 
                                "invalid url and app name not provided"};
                        AppId ->
                            {ok, Db} = couchbeam:open_or_create_db(Server, 
                                DbName, DbOptions),
                            DocId = erica_config:get_global(docid, AppId),
                            {ok, Db, AppId, DocId}
                    end
            end;
            
        Error ->
            Error
    end.

db_options_from_url(Url) ->
    case Url#url.username of
        undefined -> [];
        Username ->
            case Url#url.password of
                undefined ->
                    [{basic_auth, {Username, ""}}];
                Password ->
                    [{basic_auth, {Username, Password}}]
            end
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

%% @spec partition(String, Sep) -> {String, [], []} | {Prefix, Sep, Postfix}
%% @doc Inspired by Python 2.5's str.partition:
%%      partition("foo/bar", "/") = {"foo", "/", "bar"},
%%      partition("foo", "/") = {"foo", "", ""}.
partition(String, Sep) ->
    case partition(String, Sep, []) of
        undefined ->
            {String, "", ""};
        Result ->
            Result
    end.

partition("", _Sep, _Acc) ->
    undefined;
partition(S, Sep, Acc) ->
    case partition2(S, Sep) of
        undefined ->
            [C | Rest] = S,
            partition(Rest, Sep, [C | Acc]);
        Rest ->
            {lists:reverse(Acc), Sep, Rest}
    end.

partition2(Rest, "") ->
    Rest;
partition2([C | R1], [C | R2]) ->
    partition2(R1, R2);
partition2(_S, _Sep) ->
    undefined.

relpath(Path, Root) ->
    {_, _, RelPath} = partition(Path, Root),
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
    invalid_url.

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

%% TODO: Rename emulate_escript_foldl to escript_foldl and remove
%% this function when the time is right. escript:foldl/3 was an
%% undocumented exported fun and has been removed in R14.

escript_foldl(Fun, Acc, File) ->
    {module, zip} = code:ensure_loaded(zip),
    case erlang:function_exported(zip, foldl, 3) of
        true ->
            emulate_escript_foldl(Fun, Acc, File);
        false ->
            escript:foldl(Fun, Acc, File)
    end.



emulate_escript_foldl(Fun, Acc, File) ->
    case escript:extract(File, [compile_source]) of
        {ok, [_Shebang, _Comment, _EmuArgs, Body]} ->
            case Body of
                {source, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {beam, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {archive, ArchiveBin} ->
                    zip:foldl(Fun, Acc, {File, ArchiveBin})
            end;
        {error, _} = Error ->
            Error
    end.

find_files(Dir, Regex) ->
    filelib:fold_files(Dir, Regex, true, fun(F, Acc) -> [F | Acc] end, []).
