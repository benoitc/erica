%%% -*- erlang -*-
%%%
%%% This file is part of erica released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erica_init).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-export([init/2]).

%% ====================================================================
%% Public API
%% ====================================================================

init([], Config) ->
    init1(erica_util:get_cwd(), Config);
init([Path|_], Config) ->
    init1(Path, Config).

%% ====================================================================
%% Internal functions
%% ====================================================================

init1(Path, _Config) ->
    ok = erica_util:make_dir(Path),
    RcPath = filename:join(Path, ".couchapprc"),
    file:write_file(RcPath, ejson:encode({[]})).
