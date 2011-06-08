%%% -*- erlang -*-
%%%
%%% This file is part of erica released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erica_config).

-include("deps/couchbeam/include/couchbeam.hrl").
-include("erica.hrl").

-export([new/0, new/1,
         update/2,
         get_db/2,
         get/2, get/3,
         set/3,
         set_global/2, get_global/2, get_global/1]).
new() ->
    #config { dir = erica_util:get_cwd(),
              opts = [] }.

new(Options) ->
    UserConfFile = filename:join(erica_util:user_path(),
        ".erica.conf"),
    UserConf = case filelib:is_regular(UserConfFile) of
        true ->
            {ok, Bin} = file:read_file(UserConfFile),
            ejson:decode(Bin);
        false ->
            {[]}
    end,
    {Dbs, Hooks, Extensions, Ignore} = parse_conf(UserConf),

    #config { dir = erica_util:get_cwd(),
              opts = Options,
              dbs = Dbs,
              hooks = Hooks,
              extensions = Extensions,
              ignore = Ignore }.


update(AppDir, #config{dbs=Dbs, hooks=Hooks, extensions=Extensions,
        ignore=Ignore}=Config) ->
    RcFile = filename:join(AppDir, ".couchapprc"),

    %% load .couchapprc
    AppConf = case filelib:is_regular(RcFile) of
        true ->
            {ok, Bin} = file:read_file(RcFile),
            ejson:decode(Bin);
        false ->
            {[]}
    end,

    %% update conf from .couchapprc
    {Dbs1, Hooks1, Extensions1, Ignore1} = parse_conf(AppConf),
    Config1 = Config#config { dbs = Dbs ++ Dbs1,
                        hooks = Hooks ++ Hooks1,
                        extensions = Extensions ++ Extensions1,
                        ignore = Ignore ++ Ignore1},

    %% get ignore file patterns.
    erica_ignore:init(AppDir, Config1).


get_db(Config, DbString) ->
    proplists:get_value(DbString, Config#config.dbs).

set_global(Key, Value) ->
    application:set_env(erica_global, Key, Value).

get_global(Key) ->
    get_global(Key, undefined).

get_global(Key, Default) ->
    case application:get_env(erica_global, Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.

get(Config, Key) ->
    get(Config, Key, undefined).

get(Config, Key, Default) ->
    proplists:get_value(Key, Config#config.opts, Default).

set(Config, Key, Value) ->
    Opts = proplists:delete(Key, Config#config.opts),
    Config#config { opts = [{Key, Value} | Opts] }.

parse_conf({[]}) ->
    {[], [], [], []};
parse_conf(Conf) ->
    Dbs = case couchbeam_doc:get_value(<<"env">>, Conf) of
              undefined -> [];
              {Env}     -> Env
          end,
    Extensions = case couchbeam_doc:get_value(<<"extensions">>, Conf) of
        undefined -> [];
        {Ext} ->
            [{erica_util:v2a(Mod), erica_util:v2a(Command)}
                || {Mod, Command} <- Ext]
    end,
    Hooks = case couchbeam_doc:get_value(<<"hooks">>, Conf) of
        undefined ->
            [];
        H ->
            [{list_to_atom(Hook), Scripts} || {Hook, Scripts} <- H]
    end,
    Ignore = case couchbeam_doc:get_value(<<"ignore">>, Conf) of
        undefined ->
            [];
        I ->
            I
    end,
    {Dbs, Hooks, Extensions, Ignore}.

get_config_dbs([], Dbs) ->
    Dbs;
get_config_dbs([{Name, Obj}|Rest], Dbs) ->
    case couchbeam_doc:get_value(<<"db">>, Obj) of
        undefined ->
            get_config_dbs(Rest, Dbs);
        DbString ->
            Db = erica_util:db_from_string(binary_to_list(DbString)),
            Db1 = case couchbeam_doc:get_value(<<"oauth">>, Obj) of
                undefined ->
                    Db;
                {OauthProps} ->
                    #db{options=Options} = Db,
                    Options1 = Db#db{options=[{oauth,
                                OauthProps}|Options]},
                    Db#db{options=Options1}
            end,
            get_config_dbs(Rest, [{Name, Db1}|Dbs])
    end.
