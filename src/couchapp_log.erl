%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(couchapp_log).

-export([init/0,
         set_level/1, get_level/0,
         log/3]).

%% ===================================================================
%% Public API
%% ===================================================================

init() ->
    case couchapp_config:get_global(verbose, "0") of
        "1" ->
            set_level(debug);
        _ ->
            set_level(error)
    end.


set_level(Level) ->
    ok = application:set_env(couchapp, log_level, Level).

get_level() ->
    case application:get_env(couchapp, log_level) of
        undefined ->
            error;
        {ok, Value} ->
            Value
    end.

log(Level, Str, Args) ->
    {ok, LogLevel} = application:get_env(couchapp, log_level),
    case should_log(LogLevel, Level) of
        true ->
            io:format(log_prefix(Level) ++ Str, Args);
        false ->
            ok
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

should_log(debug, _)     -> true;
should_log(info, debug)  -> false;
should_log(info, _)      -> true;
should_log(warn, debug)  -> false;
should_log(warn, info)   -> false;
should_log(warn, _)      -> true;
should_log(error, error) -> true;
should_log(error, _)     -> false;
should_log(_, _)         -> false.

log_prefix(debug) -> "DEBUG: ";
log_prefix(info)  -> "INFO:  ";
log_prefix(warn)  -> "WARN:  ";
log_prefix(error) -> "ERROR: ".
