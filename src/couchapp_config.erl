%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couchapp_config).

-include("couchapp.hrl").

-export([new/0,
         set_global/2, get_global/2]).

new() ->
    #config { dir = couchapp_util:get_cwd(),
              opts = []}.

set_global(jobs=Key, Value) when is_list(Value) ->
    set_global(Key,list_to_integer(Value));
set_global(jobs=Key, Value) when is_integer(Value) ->
    application:set_env(couchapp_global, Key, erlang:max(1,Value));
set_global(Key, Value) ->
    application:set_env(couchapp_global, Key, Value).

get_global(Key, Default) ->
    case application:get_env(couchapp_global, Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.


