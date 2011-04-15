%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(couchapp_ignore).

-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("couchapp.hrl").

-export([init/2,
         ignore/2]).

-define(DEFAULT_IGNORE,
        [<<"^.git*">>, <<"^.DS_Store$">>, <<"\\.swp$">>, <<"~$">>,
         <<"^.svn$">>, <<"^.CVS$">>, <<"^.CVS$">>, <<"^.hg$">>]). %"

%% ====================================================================
%% Public API
%% ====================================================================

init(AppDir, #config{ignore=OldIgnore}=Config) ->
    IgnoreFile = filename:join(AppDir, ".couchappignore"),
    Ignore = case filelib:is_regular(IgnoreFile) of
        true ->
            load(IgnoreFile);
        _ ->
            []
    end,
    Config#config{ignore=OldIgnore ++ Ignore ++ ?DEFAULT_IGNORE}.

ignore(Path, #config{ignore=Ignore}) ->
    ignore1(Ignore, Path).

%% ====================================================================
%% Internal functions
%% ====================================================================

load(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Bin1 = remove_comments(Bin),
            ejson:decode(Bin1);
        Error ->
            ?ERROR("can't read '~p' [~p]~n", [File, Error]),
            []
    end.

ignore1([], _Path) ->
    false;
ignore1([Pattern|Rest], Path) ->
    case re:run(Path, Pattern, [global, caseless, unicode, multiline,
                {capture, all, binary}]) of
            nomatch ->
                ignore1(Rest, Path);
            _ ->
                ?DEBUG("File '~p' ignored.~n", [Path]),
                true
    end.

remove_comments(Content) ->
    P = "(?:/\\*(?:[^*]|(?:\\*+[^*/]))*\\*+/)|(?://.*)",
    re:replace(Content, P, "").
