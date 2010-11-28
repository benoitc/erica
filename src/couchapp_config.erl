%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couchapp_config).

-include("deps/couchbeam/include/couchbeam.hrl").
-include("couchapp.hrl").

-export([new/0,
         get_db/2,
         set_global/2, get_global/2]).

new() ->
    UserConfFile = filename:join(couchapp_util:user_path(),
        ".couchapp.conf"),
    UserConf = case filelib:is_regular(UserConfFile) of
        true ->
            {ok, Bin} = file:read_file(UserConfFile),
            couchbeam_util:json_decode(Bin);
        false ->
            {[]}
    end,
    {Dbs, Hooks, Extensions} = parse_conf(UserConf),

    #config { dir = couchapp_util:get_cwd(),
              opts = [],
              dbs = Dbs,
              hooks = Hooks,
              extensions = Extensions }.

get_db(Config, DbString) ->
    proplists:get_value(DbString, Config#config.dbs).

set_global(Key, Value) ->
    application:set_env(couchapp_global, Key, Value).

get_global(Key, Default) ->
    case application:get_env(couchapp_global, Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.

parse_conf({[]}) ->
    {[], [], []};
parse_conf(Conf) ->
    Dbs = case couchbeam_doc:get_value(<<"env">>, Conf) of
        undefined -> [];
        Env ->
            get_config_dbs({Env}, [])
    end,
    Extensions = case couchbeam_doc:get_value(<<"extensions">>, Conf) of
        undefined -> [];
        {Ext} ->
            [{couchapp_util:v2a(Mod), couchapp_util:v2a(Command)} 
                || {Mod, Command} <- Ext]
    end,
    Hooks = case couchbeam_doc:get_value(<<"Hooks">>, Conf) of
        undefined ->
            [];
        H ->
            [{list_to_atom(Hook), Scripts} || {Hook, Scripts} <- H]
    end,
    {Dbs, Hooks, Extensions}.

get_config_dbs([], Dbs) ->
    Dbs;
get_config_dbs([{Name, Obj}|Rest], Dbs) ->
    case couchbeam_doc:get_value(<<"db">>, Obj) of
        undefined ->
            get_config_dbs(Rest, Dbs);
        DbString ->
            Db = couchapp_util:db_from_string(DbString),
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
                



            


