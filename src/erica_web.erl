%%% -*- erlang -*-
%%%
%%% This file is part of erica released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erica_web).

-export([web/2, dispatch/7]).

-include_lib("erica/include/erica.hrl").
-include_lib("kernel/include/file.hrl").

-define(IMG_TYPES, [".png", ".jpeg", ".jpg", ".gif", ".ico", ".tiff",
        ".svg"]).

-define(UNQUOTE(V), mochiweb_util:unquote(V)).

-record(httpd, {
        mochi_req,
        method,
        path,
        path_parts=[],
        doc_id,
        app_name,
        app_dir,
        config = [],
        static_files,
        templates}).

web([], Config) ->
    web([erica_util:get_cwd()], Config);
web([Path], Config) ->
    Path1 = filename:absname(Path),
    case erica_util:in_couchapp(Path1) of
        {ok, CouchappDir} ->
            Config1 = erica_config:update(CouchappDir, Config),
            do_web(CouchappDir, Config1);
        {error, not_found} ->
            ?ERROR("Can't find initialized couchapp in '~p'~n", [Path]),
            halt(1)
    end.

do_web(Path, Config) ->

    %% get files from memory
    StaticFiles = cache_static(),
    Templates = cache_templates(),

    %% get port from params, default to autodetect
    Port = list_to_integer(erica_config:get_global(port, "0")),
    Ip = erica_config:get_global(ip, "127.0.0.1"),

    %% docid of document
    DocId = erica_push:id_from_path(Path, Config),
    <<"_design/", AppName/binary>> = DocId,

    %% define HTTP loop
    Loop = fun(Req)->
            apply(?MODULE, dispatch, [Req, Path, Config, DocId,
                    AppName, StaticFiles, Templates])
    end,

    Options = [
        {port, Port},
        {ip, Ip},
        {name, erica_http},
        {loop, Loop}],

    Pid = mochiweb_http:start(Options),
    PortStr = integer_to_list(mochiweb_socket_server:get(erica_http,
            port)),
    Location = binary_to_list(
        iolist_to_binary(["http://127.0.0.1:", PortStr])),

    case erica_config:get_global(browser, "0") of
        "0" -> ok;
        "false" -> ok;
        _ ->
           %% open browser if browser var is 1 or true
            erica_webbrowser:open_location(Location)
    end,

    ?CONSOLE("Erica Web started on ~p~n", [Location]),

    receive
        http_stop ->
            Pid ! stop
    end,
    ok.

dispatch(MochiReq, AppPath, Config, DocId, AppName, StaticFiles, Templates) ->
    RawUri = MochiReq:get(raw_path),
    {"/" ++ Path, _, _} = mochiweb_util:urlsplit_path(RawUri),
    PathParts = [mochiweb_util:unquote(Part) || Part <-
        string:tokens(Path, "/")],

    Req = #httpd{
        mochi_req = MochiReq,
        method = MochiReq:get(method),
        path = Path,
        path_parts = PathParts,
        doc_id = DocId,
        app_name = AppName,
        app_dir = AppPath,
        config = Config,
        static_files = StaticFiles,
        templates = Templates},
    try
        handle_request(Req)
    catch
        _:forbidden ->
            MochiReq:respond({403, [], "forbidden"});
        _:Error ->
            MochiReq:respond({500, [], io_lib:format("Error: ~p~n",
                        [Error])})
    end.



handle_request(#httpd{path_parts=[], app_name=Name, app_dir=Dir}=Req) ->
    Tree = [dict:from_list(Props) || Props <- tree(Dir, Dir)],
    Actions = render(Req, "tree_actions.html", [{rel_path, ""}]),

    render_template(Req, "index.html",
        [{app_name, binary_to_list(Name)},
         {actions, Actions},
         {tree, Tree}]);

handle_request(#httpd{path_parts=["push"], app_dir=Dir, config=Config,
        doc_id=DocId, mochi_req=MochiReq}) ->

    case MochiReq:get(method) of
        'POST' ->
            case MochiReq:get_primary_header_value("content-type") of
                "application/json" ->
                    {Props} = couchbeam_ejson:decode(MochiReq:recv_body()),
                    DbKey = proplists:get_value(<<"url">>, Props),
                    Db = erica_util:db_from_key(Config,
                        binary_to_list(DbKey)),
                    try erica_push:do_push(Dir, Db, DocId, Config) of
                        {ok, DisplayUrl0} ->
                            DisplayUrl = iolist_to_binary(DisplayUrl0),
                            JsonObj = {[
                                    {<<"ok">>, true},
                                    {<<"url">>, DisplayUrl}]},
                             MochiReq:ok({<<"application/json">>, [],
                                     couchbeam_ejson:encode(JsonObj)})
                    catch
                        _:Reason ->
                            ?ERROR("web push error: ~p~n", [Reason]),
                            json_error(MochiReq, 400, <<>>)
                    end;
                _ ->
                    json_error(MochiReq, 406, "Content-Type not
                        accepted")
            end;
        _ ->
            json_error(MochiReq, 405, "only POST is accepted")
    end;


handle_request(#httpd{path_parts=["upload"], app_dir=Dir, mochi_req=MochiReq}) ->
    case MochiReq:get(method) of
        'POST' ->
            Form = mochiweb_multipart:parse_form(MochiReq),
            ?INFO("got ~p~n", [Form]),
            RelPath = proplists:get_value("path", Form),

            {Name, {_ContentType, _}, Bin} = proplists:get_value("file",
                Form),

            FName = RelPath ++ "/" ++ Name,
            AName = ?UNQUOTE(filename:absname(FName, Dir)),
            ?DEBUG("Upload file ~p~n", [AName]),
            ok = check_path(AName, Dir),
            case filelib:ensure_dir(AName) of
                ok ->
                    ok;
                _ ->
                    DirName = filename:dirname(AName),
                    ?DEBUG("Create dir ~p~n",
                        [DirName]),
                    os:cmd("mkdir -p " ++ DirName)
            end,
            case filelib:is_file(AName) of
                true ->
                    ok;
                _ ->
                    ok = file:write_file(AName, Bin)
            end,

            json_ok(MochiReq);
        _ ->
            json_error(MochiReq, 405, "only POST is accepted")
    end;

handle_request(#httpd{path_parts=["create"], app_dir=Dir, mochi_req=MochiReq}) ->
    case MochiReq:get(method) of
        'POST' ->
            case MochiReq:get_primary_header_value("content-type") of
                "application/json" ->
                    {Props0} = couchbeam_ejson:decode(MochiReq:recv_body()),
                    Actions = proplists:get_value(<<"actions">>, Props0),
                    ?DEBUG("actions ~p~n", [Actions]),

                    lists:foreach(fun
                        ({[{<<"dir">>, {Props}}]}) ->
                            DName = proplists:get_value(<<"dirname">>,
                                Props),
                            DName1 = ?UNQUOTE(binary_to_list(DName)),
                            AName = filename:absname(DName1, Dir),
                            ?DEBUG("Create dir ~p~n", [AName]),
                            ok = check_path(AName, Dir),
                            os:cmd("mkdir -p " ++ AName);
                        ({[{<<"file">>, {Props}}]}) ->
                            FName = proplists:get_value(<<"filename">>,
                                Props),
                            FName1 = ?UNQUOTE(binary_to_list(FName)),
                            AName = filename:absname(FName1, Dir),
                            ?DEBUG("Create file ~p~n", [AName]),
                            ok = check_path(AName, Dir),

                            case filelib:ensure_dir(AName) of
                                ok ->
                                    ok;
                                _ ->
                                    DirName = filename:dirname(AName),
                                    ?DEBUG("Create dir ~p~n",
                                        [DirName]),
                                    os:cmd("mkdir -p " ++ DirName)
                            end,
                            case filelib:is_file(AName) of
                                true ->
                                    ok;
                                _ ->
                                    Content = proplists:get_value(<<"content">>,
                                        Props, <<"">>),

                                    ok = file:write_file(AName, Content)
                            end;
                        (_) ->
                            ok
                    end, Actions),
                    json_ok(MochiReq);
                _ ->
                    json_error(MochiReq, 406, "Content-Type not
                        accepted")
            end;
        _ ->
            json_error(MochiReq, 405, "only POST is accepted")
    end;

handle_request(#httpd{path_parts=["delete"], app_dir=Dir, mochi_req=MochiReq}) ->
    case MochiReq:get(method) of
        'POST' ->
            case MochiReq:get_primary_header_value("content-type") of
                "application/json" ->
                    {Props} = couchbeam_ejson:decode(MochiReq:recv_body()),
                    Files = proplists:get_value(<<"files">>, Props),
                    lists:foreach(fun(File) ->
                               FName =
                               ?UNQUOTE(filename:absname(binary_to_list(File),
                                       Dir)),
                               ?INFO("fname to delete ~p~n", [FName]),
                               ok = check_path(FName, Dir),

                               Out = os:cmd("rm -rf \"" ++ FName ++ "\""),
                               ?DEBUG("delete output ~p~n", [Out])
                       end, Files),
                    json_ok(MochiReq);
                _ ->
                    json_error(MochiReq, 406, "Content-Type not
                        accepted")
            end;
        _ ->
            json_error(MochiReq, 405, "only POST is accepted")
    end;

handle_request(#httpd{path_parts=["tree"], mochi_req=MochiReq}) ->
    Location = absolute_uri(MochiReq, "/"),
    MochiReq:respond({301, [{"Location", Location}], <<>>});

handle_request(#httpd{path_parts=["tree"|PathParts], app_name=Name,
        app_dir=Dir}=Req) ->
    FName = filename:join([Dir|PathParts]),
    ok = check_path(FName, Dir),

    {Tmpl, Ctx} = case filelib:is_dir(FName) of
        true ->
            RelPath = fix_evently(string:join(PathParts, "/")),
            Actions = render(Req, "tree_actions.html", [{rel_path, RelPath}]),
            BreadCrumb = breadcrumb(PathParts),
            Tree = [dict:from_list(Props) || Props <- tree(FName, Dir)],

            Ctx0 = [{app_name, binary_to_list(Name)},
                           {tree, Tree},
                           {breadcrumb, BreadCrumb},
                           {actions, Actions}],
            {"tree.html", Ctx0};
        false ->
            [FileName|Rest] = lists:reverse(PathParts),
            BreadCrumb = breadcrumb(lists:reverse(Rest)),

            Ext = filename:extension(FName),

            {ok, #file_info{size=S, mode=M}} = file:read_file_info(FName),

            RelPath = fix_evently("/" ++ string:join(PathParts, "/")),
            Href = "/raw" ++ RelPath,

            Extra = case lists:member(Ext, ?IMG_TYPES) of
                true ->
                    Content = render(Req, "file_img.html", [
                            {href, Href},
                            {size, S},
                            {mode, M}]),
                    [{content, Content}];

                false ->
                    {ok, Bin0} = file:read_file(FName),
                    Bin = mochiweb_html:escape(Bin0),
                    Content = render(Req, "file.html", [
                            {bin, Bin},
                            {rel_path, RelPath},
                            {href, Href},
                            {size, S},
                            {mode, M}]),
                    Actions = render(Req, "file_actions.html", [
                            {rel_path, RelPath},
                            {href, Href}]),
                    [{content, Content}, {actions, Actions}]
            end,

            Ctx0 = [{app_name, binary_to_list(Name)},
                    {file_name, FileName},
                    {rel_path, RelPath},
                    {breadcrumb, BreadCrumb}] ++ Extra,
            {"tree_file.html", Ctx0}
    end,

    render_template(Req, Tmpl, Ctx);

handle_request(#httpd{method='GET', path_parts=["edit"|PathParts],
        app_name=Name, app_dir=Dir, mochi_req=MochiReq, path=Path}=Req) ->

    FName = ?UNQUOTE(filename:join([Dir|PathParts])),
    ok = check_path(FName, Dir),

    case filelib:is_dir(FName) of
        true ->
            MochiReq:respond({"415", [], "A directory can't be
                    edited"});
        false ->
            [FileName|Rest] = lists:reverse(PathParts),
            BreadCrumb = breadcrumb(lists:reverse(Rest)),
            Ext = filename:extension(FName),
            RelPath = fix_evently("/" ++ string:join(PathParts, "/")),
            {ok, Bin0} = file:read_file(FName),
            Bin = mochiweb_html:escape(Bin0),
            %% find ace mode
            Mode = ace_mode(Ext),
            render_template(Req, "edit.html", [
                    {app_name, binary_to_list(Name)},
                    {path, "/" ++ Path},
                    {file_name, FileName},
                    {rel_path, RelPath},
                    {bin, Bin},
                    {mode, Mode},
                    {breadcrumb, BreadCrumb}])
    end;

handle_request(#httpd{method='POST', path_parts=["edit"|PathParts],
        app_dir=Dir, mochi_req=MochiReq}) ->

    FName = ?UNQUOTE(filename:join([Dir|PathParts])),
    ok = check_path(FName, Dir),

    case filelib:is_dir(FName) of
        true ->
            json_error(MochiReq, 415, "A directory can't be edited");
        false ->
            ReqBody = MochiReq:recv_body(),
            case MochiReq:get_primary_header_value("content-type") of
                "application/json" ->
                    {Props} = couchbeam_ejson:decode(ReqBody),

                    Data = proplists:get_value(<<"data">>,
                            Props, ""),
                    case file:write_file(FName, Data) of
                        ok ->
                            json_ok(MochiReq);

                        {error, Error} ->
                            Reason = io_lib:format("Server error: ~p",
                                [Error]),
                            json_error(MochiReq, 500, Reason)
                    end;
                _ ->
                    json_error(MochiReq, 406, "Content-Type not
                        accepted")
            end
    end;


handle_request(#httpd{path_parts=["raw"|PathParts],
        mochi_req=MochiReq, app_dir=Dir}) ->
    Path = ?UNQUOTE(string:join(PathParts, "/")),
    MochiReq:serve_file(Path, Dir);

handle_request(#httpd{path=Path}=Req) ->
    serve_file(Req, ?UNQUOTE(Path)).


json_respond(MochiReq, Status, JsonObj) ->
    json_respond(MochiReq, Status, [], JsonObj).

json_respond(MochiReq, Status, ExtraHeaders, JsonObj) ->
    Headers = [{"Content-Type", "application/json"}] ++ ExtraHeaders,
    MochiReq:respond({Status, Headers, couchbeam_ejson:encode(JsonObj)}).


json_error(MochiReq, Status, Error) when is_list(Error) ->
    json_error(MochiReq, Status, list_to_binary(Error));
json_error(MochiReq, Status, Error) ->
    JsonObj = {[
            {<<"error">>, true},
            {<<"reason">>, Error}]},
    json_respond(MochiReq, Status, JsonObj).

json_ok(MochiReq) ->
    json_ok(MochiReq, []).

json_ok(MochiReq, Extra) ->
    JsonProps = [{<<"ok">>, true}] ++ Extra,
    MochiReq:ok({<<"application/json">>, [],
            couchbeam_ejson:encode({JsonProps})}).

host_headers(MochiReq) ->
    [ V || V <- [MochiReq:get_header_value(H)
                             || H <- ["x-forwarded-host",
                                      "x-forwarded-server",
                                      "host"]],
           V /= undefined].

absolute_uri(MochiReq, Path) ->
    Host = case host_headers(MochiReq) of
        [H|_] ->
            H;
        _ ->
            {ok, {Address, Port}} = inet:sockname(MochiReq:get(socket)),
            inet_parse:ntoa(Address) ++ ":" ++ integer_to_list(Port)
    end,
    "http://" ++ Host ++ Path.

render(#httpd{templates=Templates},Name, Ctx0) ->
    Ctx = dict:from_list(Ctx0),
    case lists:keyfind(Name, 1, Templates) of
    {Name, {_FileInfo, Bin}} ->
        %% Be sure to escape any double-quotes before rendering...
        ReOpts = [global, {return, list}],
        Str0 = re:replace(Bin, "\\\\", "\\\\\\", ReOpts),
        Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
        Rendered = mustache:render(Str1, Ctx),

        %% inneficient method to remove trailing new line.
        case lists:reverse(Rendered) of
            [$\n|Rendered1] ->
                lists:reverse(Rendered1);
            _ ->
                Rendered
        end;
    _ ->
        ""
    end.


render_template(Req, Name, Ctx0) ->
    Ctx = dict:from_list(Ctx0),

    #httpd{mochi_req=MochiReq, templates=Templates}=Req,
    Res = case lists:keyfind(Name, 1, Templates) of
        {Name, {_FileInfo, Bin}} ->
            {Name, Bin};
        _ ->
            Path = filname:join([priv_dir(), "web_templates", Name]),
            case filelib:is_file(Path) of
                true ->
                    {ok, Bin} = file:read_file(Path),
                    {Name, Bin};
                false ->
                    not_found
            end
    end,
    case Res of
        {Name, Bin0} ->
            %% Be sure to escape any double-quotes before rendering...
            ReOpts = [global, {return, list}],
            Str0 = re:replace(Bin0, "\\\\", "\\\\\\", ReOpts),
            Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
            Rendered = mustache:render(Str1, Ctx),

            %% inneficient method to remove trailing new line.
            Bin1 = case lists:reverse(Rendered) of
                [$\n|Rendered1] ->
                    lists:reverse(Rendered1);
                _ ->
                    Rendered
            end,
            ContentType = mochiweb_util:guess_mime(Name),
            MochiReq:ok({ContentType, [], Bin1});
        _ ->
            MochiReq:not_found()
    end.

serve_file(#httpd{mochi_req=MochiReq, static_files=Files}, Name) ->
    case lists:keyfind(Name, 1, Files) of
        {Name, {FileInfo, Bin}} ->
            serve_file(MochiReq, Name, FileInfo, Bin);
        _ ->
            Path = filname:join([priv_dir(), "web_static", Name]),
            case filelib:is_file(Path) of
                true ->
                    {ok, Bin} = file:read_file(Path),
                    {ok, FileInfo} = file:read_file_info(Path),
                    serve_file(MochiReq, Name, FileInfo, Bin);
                false ->

                    MochiReq:not_found()
            end
    end.

serve_file(MochiReq, Name, FileInfo, Bin) ->
    LastModified = httpd_util:rfc1123_date(FileInfo#file_info.mtime),
    case MochiReq:get_header_value("if-modified-since") of
        LastModified ->
            MochiReq:respond({304, [], ""});
        _ ->
            ContentType = mochiweb_util:guess_mime(Name),
            MochiReq:ok({ContentType,  [{"last-modified", LastModified}],
                         Bin})
    end.

priv_dir() ->
    case code:priv_dir(ericq) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir ->
            Dir
    end.

cache_templates() ->
    {ok, RegExp} = re:compile(<<"^priv\/web_templates\/(.*)$">>, []),
    cache_escript_files(RegExp).

cache_static() ->
    {ok, RegExp} = re:compile(<<"^priv\/web_static\/(.*)$">>, []),
    cache_escript_files(RegExp).

cache_escript_files(RegExp) ->
    {ok, Files} = erica_util:escript_foldl(
                    fun(Name0, GetInfo, GetBin, Acc) ->
                            case re:run(Name0, RegExp, [{capture,[1],list}]) of
                                {match, [Name]} ->
                                    [{Name, {GetInfo(), GetBin()}} |
                                        Acc];
                                _ -> Acc
                            end
                    end,
                    [], erica_config:get_global(escript, undefined)),
    Files.

tree(Path, AppDir) ->
    ?DEBUG("make tree for ~p~n", [Path]),
    AllFiles = filelib:wildcard("*", Path),

    {Dirs, Files} = lists:foldl(fun
                (".", Acc) -> Acc;
                ("..", Acc) -> Acc;
                (Name, {D, F}=Acc) ->
                    Name1 = filename:join(Path, Name),
                    RelPath = erica_util:relpath(Name1, AppDir),
                    case file:read_file_info(Name1) of
                    {ok, #file_info{mtime=T}} ->
                        [MTime] = calendar:local_time_to_universal_time_dst(T),
                        case filelib:is_dir(Name1) of
                            true ->
                                Item = tree_item(Name,
                                    fix_evently(RelPath),
                                    "dir","dir.png", MTime),
                                {[Item|D], F};
                            false ->
                                Item = tree_item(Name,
                                    fix_evently(RelPath),
                                    "file", "txt.png", MTime),
                                {D, [Item|F]}
                        end;
                    {error, _} ->
                        ?ERROR("can't read ~p~n", [Name1]),
                        Acc
                    end
            end, {[], []}, AllFiles),
    lists:reverse(Dirs) ++ lists:reverse(Files).


tree_item(Name, Href, Alt, Icon, MTime) ->
    [{name, Name},
     {href, Href},
     {alt, Alt},
     {icon, Icon},
     {mtime, httpd_util:rfc1123_date(MTime)}].

breadcrumb(PathParts) ->
    {_, BreadCrumb} = lists:foldl(fun(Path, {Href, Paths}) ->
                Href1 = Href ++ Path ++ "/",
                Active = if length(Paths) =:= length(PathParts) -> "active";
                    true -> ""
                end,

                Prop = [{href, fix_evently(Href1)},
                        {name, Path},
                        {active, Active}],
                {Href1, [dict:from_list(Prop)|Paths]}
        end, {"/tree/", []}, PathParts),
    lists:reverse(BreadCrumb).


ace_mode(".clj") ->
    "clojure";
ace_mode(".coffee") ->
    "coffee";
ace_mode(".css") ->
    "css";
ace_mode(".htm") ->
    "html";
ace_mode(".html") ->
    "html";
ace_mode(".java") ->
    "java";
ace_mode(".js") ->
    "javascript";
ace_mode(".json") ->
    "json";
ace_mode(".lua") ->
    "lua";
ace_mode(".md") ->
    "markdown";
ace_mode(".py") ->
    "python";
ace_mode(".rb") ->
    "ruby";
ace_mode(".xml") ->
    "xml";
ace_mode(".rdf") ->
    "xml";
ace_mode(".scala") ->
    "scala";
ace_mode(".svg") ->
    "svg";
ace_mode(".sh") ->
    "powershell";
ace_mode(_) ->
    "plain".

%% check if path is inside the couchapp, if not throw an error.
check_path(Path, CouchappDir) when is_list(CouchappDir) ->
    check_path(Path, list_to_binary(CouchappDir));
check_path(Path, CouchappDir) ->
    Path1 = list_to_binary(erica_util:normalize_path(Path)),
    S = size(CouchappDir),
    case Path1 of
        <<CouchappDir:S/binary, _/binary>> ->

            ok;
        _ ->
            throw({forbidden, Path1})
    end.

%% Evently directory can start with a # ....
fix_evently(P) ->
    re:replace(P, "\#", "%23", [{return, list}]).
