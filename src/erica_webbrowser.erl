%%% -*- erlang -*-
%%%
%%% This file is part of erica released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erica_webbrowser).

-export([browse/2]).
-export([open_location/1]).

-include_lib("erica/include/erica.hrl").
-include_lib("couchbeam/include/couchbeam.hrl").

-define(UNIX_BROWSERS, ["mozilla-firefox", "firefox",
        "mozilla-firebird", "firebird", "seamonkey", "mozilla",
        "netscape", "galeon", "epiphany", "skipstone", "opera",
        "mosaic","chromium"]).
-define(UNIX_TERM_BROWSERS, ["links", "elinks", "lynx", "w3m"]).
-define(WIN_BROWSERS, ["firefox", "firebird", "seamonkey", "mozilla",
        "netscape", "opera","chrome"]).


browse([], Config) ->
    browse(["default"], Config);
browse([DbKey], Config) ->
    browse1(erica_util:get_cwd(), DbKey, Config);

browse([Path, DbKey|_], Config) ->
    browse1(Path, DbKey, Config).

browse1(Path, DbKey, Config) ->
    Path1 = filename:absname(Path),
    case erica_util:in_couchapp(Path1) of
        {ok, CouchappDir} ->
            %% load app conf from .couchapprc and initialize ignore
            %% patterns.
            Config1 = erica_config:update(CouchappDir, Config),

            Db = erica_util:db_from_key(Config1, DbKey),
            ?DEBUG("browse ~p to ~p~n", [DbKey, CouchappDir]),
            do_browse(CouchappDir, Db, Config1);

        {error, not_found} ->
            ?ERROR("Can't find initialized couchapp in '~p'~n", [Path]),
            halt(1)
    end.


do_browse(Path, Db, Config) ->
    DocId = erica_push:id_from_path(Path, Config),
    do_browse(Path, Db, DocId, Config).

do_browse(Path, #db{server=Server}=Db, DocId, Config) ->
    OldDoc = case couchbeam:open_doc(Db, DocId) of
        {ok, OldDoc1} ->
            OldDoc1;
        {error, not_found} ->
            {[]}
    end,
    
    Couchapp = #couchapp{
        config=Config,
        path=Path,
        att_dir=filename:join(Path, "_attachments"),
        docid=DocId,
        doc={[{<<"_id">>, DocId}]},
        old_doc = OldDoc
    },
    {ok, Couchapp1} = erica_push:couchapp_from_fs(Couchapp),

    CouchappUrl = couchbeam:make_url(Server, couchbeam:doc_url(Db, DocId), []),
    BrowseUrl = erica_push:index_url(CouchappUrl, Couchapp1),
    open_location(BrowseUrl).
    

open_location(Location) when is_binary(Location) ->
    open_location(binary_to_list(Location));
open_location(Location) ->
    open_location(os:type(), Location).


open_location({unix, darwin}, Location) ->
    Cmd = "open " ++ re:replace(Location, "\"", "%22",
        [{return,list}]),
    erica_util:sh(Cmd, erica_util:os_env());
open_location({unix, _}, Location) ->
    launch_unix_browser(Location);
open_location(_, Location) ->
    Cmd = "open " ++ re:replace(Location, "\"", "%22", [{return,list}]),
    erica_util:sh(Cmd).
    
launch_unix_browser(Location) ->
    Env = erica_util:os_env(),
    case proplists:get_value("DISPLAY", Env) of
        undefined ->
            find_browser(?UNIX_TERM_BROWSERS, unix, Location);
        _ ->
            launch_x11_unix_browser(Env, Location)
    end.

launch_x11_unix_browser(Env, Location) ->
    case proplists:get_value("GNOME_DESKTOP_SESSION_ID", Env) of
	undefined ->
	    case proplists:get_value("GNOME_DESKTOP_SESSION_ID", Env) of
	    undefined ->
		find_browser(?UNIX_BROWSERS, unix, Location);
	    _ ->
		case erica_util:find_executable(kfmclient) of
		{ok, Path} ->
		    Cmd = Path ++ " openUrl " ++ Location,
		    erica_util:sh(Cmd, erica_util:os_env());
		false ->
		    find_browser(?UNIX_BROWSERS, unix, Location)
            end
        end;
	_ ->
	    case erica_util:find_executable("gnome-open") of
	    {ok, Path} ->
		generic_browser_cmd(Path, Location);
	    false ->
		find_browser(?UNIX_BROWSERS, unix, Location)
        end
    end.


find_browser([], _Type, _Location) ->
    ?ERROR("Can't find a browser", []),
    halt(1);
find_browser([Name|Rest], Type, Location) ->
    case erica_util:find_executable(Name) of
	false ->
	    find_browser(Rest, Type, Location);
	{ok, Path} ->
	    case Type of
		unix ->
		    unix_browser_cmd(Name, Path, Location);
		_ ->
		    generic_browser_cmd(Path, Location)
	    end;
	Local ->
	    case Type of
		unix ->
		    catch unix_browser_cmd(Name, Local, Location);
		_ ->
		    generic_browser_cmd(Local, Location)
	    end		    
    end.

unix_browser_cmd("mozilla-firefox", Path, Location) ->
    mozilla_browser_cmd(Path, Location);
unix_browser_cmd("firefox", Path, Location) ->
    mozilla_browser_cmd(Path, Location);
unix_browser_cmd("mozilla-firebird", Path, Location) ->
    mozilla_browser_cmd(Path, Location);
unix_browser_cmd("firebird", Path, Location) ->
    mozilla_browser_cmd(Path, Location);
unix_browser_cmd("seamonkey", Path, Location) ->
    mozilla_browser_cmd(Path, Location);
unix_browser_cmd("mozilla", Path, Location) ->
    mozilla_browser_cmd(Path, Location);
unix_browser_cmd("netscape", Path, Location) ->
    mozilla_browser_cmd(Path, Location);
unix_browser_cmd("galeon", Path, Location) ->
    galeon_browser_cmd(Path, Location);
unix_browser_cmd("epiphany", Path, Location) ->
    galeon_browser_cmd(Path, Location);
unix_browser_cmd("skipstone", Path, Location) ->
    generic_browser_cmd(Path, Location);
unix_browser_cmd("opera", Path, Location) ->
    Cmd = Path ++ " " ++ Location ++ " -w",
    erica_util:sh(Cmd, []);
unix_browser_cmd("mosaic", Path, Location) ->
    generic_browser_cmd(Path, Location);
unix_browser_cmd("links", Path, Location) ->
    generic_browser_cmd(Path, Location);
unix_browser_cmd("lynx", Path, Location) ->
    generic_browser_cmd(Path, Location);
unix_browser_cmd("w3m", Path, Location) ->
    generic_browser_cmd(Path, Location);
unix_browser_cmd("chromium", Path, Location) ->
    generic_browser_cmd(Path, Location);
unix_browser_cmd("chrome", Path, Location) ->
    generic_browser_cmd(Path, Location);
unix_browser_cmd("elinks", Path, Location) ->
    mozilla_browser_cmd(Path, Location).


generic_browser_cmd(Path, Location) ->
    Cmd = Path ++ " " ++ Location,
    erica_util:sh(Cmd, erica_util:os_env()).

mozilla_browser_cmd(Path, Location) ->
    Cmd = Path ++ "-remote openUrl(" ++ Location ++ ",new-window)",
    erica_util:sh(Cmd, erica_util:os_env()).

galeon_browser_cmd(Path, Location) ->
    Cmd = Path ++ "-w " ++ Location,
    erica_util:sh(Cmd, erica_util:os_env()).
