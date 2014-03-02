%%% -*- erlang -*-
%%%
%%% This file is part of erica released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erica_core).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include_lib("erica/include/erica.hrl").

-export([run/1]).


start_erica() ->
    case application:load(erica) of
        ok ->
            ok;
        {error, {already_loaded, _}} ->
            ok;
        Error ->
            Error
    end.

run(["compile"]) ->
    ok;
run(["help"]) ->
    help(),
    ok;
run(["version"]) ->
    ok = start_erica(),
    version(),
    ok;
run(RawArgs) ->
    ok = start_erica(),

    %% parse arguments
    {Options, Commands} = parse_args(RawArgs),

    %% load couchbeam
    ok = hackney:start(),
    ok = couchbeam:start(),

    %% Initialize logging system
    erica_log:init(),

    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    erica_config:set_global(escript, filename:absname(escript:script_name())),
    ?DEBUG("Couchapp location: ~p\n", [erica_config:get_global(escript, undefined)]),

    %% Note the top-level directory for reference
    erica_config:set_global(base_dir, filename:absname(erica_util:get_cwd())),

    process_commands(Commands, Options).

process_commands([Command|Args], Options) ->
    {ok, Modules} = application:get_env(erica, modules),
    Config = erica_config:new(Options),
    execute(list_to_atom(Command), Args, Modules, Config).

execute(Command, Args, Modules, Config) ->
    case select_modules(Modules, Command, []) of
        [] ->
            ?WARN("'~p' unkown command", [Command]);
        TargetModules ->
            Dir = erica_util:get_cwd(),
            ?CONSOLE("==> ~s (~s)\n", [filename:basename(Dir), Command]),
            case catch(run_modules(TargetModules, Command, Args, Config)) of
                {ok, _} ->
                    ok;
                ok ->
                    ok;
                {error, failed} ->
                    ?FAIL;
                {Module, {error, _} = Other} ->
                    ?ABORT("~p failed while processing ~s in module ~s: ~s\n",
                           [Command, Dir, Module, io_lib:print(Other, 1,80,-1)]);
                Other ->
                    ?ABORT("~p failed while processing ~s: ~s\n",
                           [Command, Dir, io_lib:print(Other, 1,80,-1)])
            end
    end.

select_modules([], _Command, Acc) ->
    lists:reverse(Acc);
select_modules([Module | Rest], Command, Acc) ->
    Exports = Module:module_info(exports),
    case lists:member({Command, 2}, Exports) of
        true ->
            select_modules(Rest, Command, [Module | Acc]);
        false ->
            select_modules(Rest, Command, Acc)
    end.

run_modules([], _Command, _Args, _Config) ->
    ok;
run_modules([Module | Rest], Command, Args, Config) ->
    case Module:Command(Args, Config) of
        ok ->
            run_modules(Rest, Command, Args, Config);
        {error, _} = Error ->
            {Module, Error}
    end.

parse_args(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            {ok, continue} = show_info_maybe_halt(Options, NonOptArgs),

            %% Set global variables based on getopt options
            set_global_flag(Options, verbose),
            set_global_flag(Options, force),
            set_global_flag(Options, webstyle),

            %% Filter all the flags (i.e. strings of form key=value) from the
            %% command line arguments. What's left will be the commands to run.
            {Options, filter_flags(NonOptArgs, [])};
        {error, {Reason, Data}} ->
            ?ERROR("Error: ~s ~p~n~n", [Reason, Data]),
            help(),
            halt(1)
    end.

%%
%% options accepted via getopt
%%
option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",       undefined, "Show the program options"},
     {commands, $c, "commands",   undefined, "Show available commands"},
     {verbose,  $v, "verbose",    undefined, "Be verbose about what gets done"},
     {force,    $f, "force",      undefined, "Force"},
     {webstyle, $w, "webstyle",   undefined, "Web style project."},
     {version,  $V, "version",    undefined, "Show version information"},
     {is_ddoc, undefined, "is-ddoc", {boolean, true}, "Tell to push command if you send a design document or not."},
     {docid, undefined, "docid",  string, "Set docid with push command"},
     {atomic, undefined, "atomic",  {boolean, true},
         "Send attachments inline with push command"}

    ].

%%
%% set global flag based on getopt option boolean value
%%
set_global_flag(Options, Flag) ->
    Value = case proplists:get_bool(Flag, Options) of
                true ->
                    "1";
                false ->
                    "0"
            end,
    erica_config:set_global(Flag, Value).

%%
%% show info and maybe halt execution
%%
show_info_maybe_halt(Opts, NonOptArgs) ->
    case proplists:get_bool(help, Opts) of
        true ->
            help(),
            halt(0);
        false ->
            case proplists:get_bool(commands, Opts) of
                true ->
                    commands(),
                    halt(0);
                false ->
                    case proplists:get_bool(version, Opts) of
                        true ->
                            version(),
                            halt(0);
                        false ->
                            case NonOptArgs of
                                [] ->
                                    ?CONSOLE("No command to run specified!~n",[]),
                                    help(),
                                    halt(1);
                                _ ->
                                    {ok, continue}
                            end
                    end
            end
    end.
help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "erica",
                 "[...] <command,...>",
                 [{"command", "Command to run (e.g. push)"}]).
%%
%% print known commands
%%
commands() ->
    S = <<"
push           [options...] [dir] dest  push a document to couchdb
create-webapp  [appid=myapp] ...        Create a webapp. Default:
                                        appid=myapp, lang=javascript
create-app     [appid=myapp] ...        Create a blank ddoc, Default:
                                        appid=myapp, lang=javascript
create         template= [vars...]      create an application using a
                                        template
init                                    initialize a .couchapprc
clone          [option] source dir      clone a document from couchdb
browse                                  display the erica in the
                                        browser.
web            port=Port [dir]          launch the web ui
help                                    Show the program options
version                                 Show version information

">>,
    io:put_chars(S),
    %% workaround to delay exit until all output is written
    timer:sleep(300).




%%
%% show version information and halt
%%
version() ->
    {ok, Vsn} = application:get_key(erica, vsn),
    ?CONSOLE("erica version: ~s\n", [Vsn]).


%%
%% Seperate all commands (single-words) from flags (key=value) and store
%% values into the rebar_config global storage.
%%
filter_flags([], Commands) ->
    lists:reverse(Commands);
filter_flags([Item | Rest], Commands) ->
    case string:tokens(Item, "=") of
        [Command] ->
            filter_flags(Rest, [Command | Commands]);
        [KeyStr, Value] ->
            Key = list_to_atom(KeyStr),
            erica_config:set_global(Key, Value),
            filter_flags(Rest, Commands);
        Other ->
            ?CONSOLE("Ignoring command line argument: ~p\n", [Other]),
            filter_flags(Rest, Commands)
    end.
