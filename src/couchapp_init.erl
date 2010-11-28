%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couchapp_init).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-export([init/2]).

%% ====================================================================
%% Public API
%% ====================================================================

init([], Config) ->
    init1(couchapp_util:get_cwd(), Config);
init([Path|_], Config) ->
    init1(Path, Config).

%% ====================================================================
%% Internal functions
%% ====================================================================

init1(Path, _Config) ->
    case filelib:is_dir(Path) of
        true ->
            ok;
        false ->
            file:make_dir(Path)
    end,
    RcPath = filename:join(Path, ".couchapprc"),
    file:write_file(RcPath, couchbeam_util:json_encode({[]})).

