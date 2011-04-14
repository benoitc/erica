%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(couchapp_generate).

generate([AppName]) ->
    generate_app(AppName, ".");

generate([AppName, AppPath]) ->
    generate_app(AppName, AppPath).

%% ===================================================================
%% Internal functions
%% ===================================================================

generate_app(AppName, Path) ->
    Path1 = filename:abspath(filename:join(file:get_cwd(), Path)),

    %% create app dir
    ok = file:make_dir(Path1),

    %% initiate default paths
    lists:foreach(fun(P) ->
            P1 = filename:join(Path1, P),
            ok = file:make_dir(P1)
        end, ["_attachments", "shows", "lists", "views"]).
