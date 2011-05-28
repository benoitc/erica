%%% -*- erlang -*-
%%%
%%% This file is part of erlca released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erlca_clone).

-include("erlca.hrl").

-export([clone/2]).

%% ====================================================================
%% Public API
%% ====================================================================

clone([Url], Config) ->
    clone1(".", Url, Config);
clone([Url, Path|_], Config) ->
    clone1(filename:absname(Path), Url, Config);
clone(_, _) ->
    ?ERROR("missing arguments. Command line should be :'erlca clone "
       ++ "URL [CouchappDir]", []),
    halt(1).

%% ====================================================================
%% Internal functions
%% ====================================================================

clone1(Path, Url, Config) ->
    case erlca_util:parse_erlca_url(Url) of
        {ok, Db, AppName, DocId} ->
            Path1 = case Path of
                "." ->
                    filename:join(erlca_util:get_cwd(), AppName);
                _ ->
                    filename:absname(Path)
            end,
            case erlca_util:in_erlca(Path1) of
                {ok, _} ->
                    ?ERROR("Can't clone in an existing erlca.~n",
                        []),
                    halt(1);
                _ ->
                do_clone(Path1, DocId, Db, Config)
            end;
        Error ->
            ?ERROR("clone: ~p~n", [Error])
    end.

do_clone(Path, DocId, Db, Config) ->
    case couchbeam:open_doc(Db, DocId) of
        {ok, Doc} ->
            % initialize the erlca directory
            ok = filelib:ensure_dir(Path),
            ?DEBUG("path ~p~n", [Path]),
            erlca_init:init([Path], Config),

            AttDir = filename:join(Path, "_attachments"),
            {Atts} = couchbeam_doc:get_value(<<"_attachments">>, Doc,
                {[]}),
            Atts1 = [AName || {AName, _} <- Atts],
            Meta = couchbeam_doc:get_value(<<"couchapp">>, Doc, {[]}),
            Manifest = couchbeam_doc:get_value(<<"manifest">>, Meta),

            %% get extension associated
            Manifest1 = lists:foldl(fun(P, Acc) ->
                        P1 = binary_to_list(P),
                        case filename:extension(P1) of
                            [] ->
                                Acc;
                            Ext ->
                                PropPath = filename:join([
                                        Path,
                                        filename:dirname(P1),
                                        filename:basename(P1, Ext)]),
                                [{PropPath, Ext}|Acc]
                        end
                end, [], Manifest),

            {Objects} = couchbeam_doc:get_value(<<"objects">>, Meta,
                {[]}),
            % save doc to the fs, create approriate paths if needed
            {DocProps} = Doc,
            ok = doc_to_fs(DocProps, Path, Manifest1, Objects, 0),

            % save attachments
            ok = attachments_to_fs(Atts1, Db, DocId, AttDir),
            ?INFO("clone success. ~n", []);
        Error ->
            ?ERROR("clone error: [~p]~n", [Error]),
            halt(1)
    end,
    ok.

attachments_to_fs([], _Db, _DocId, _AttDir) ->
    ok;
attachments_to_fs([AttName|Rest], Db, DocId, AttDir) ->
    AttName1 = binary_to_list(AttName),
    Path = filename:join(AttDir, filename:nativename(AttName1)),
    Dir = filename:dirname(Path),
    ok = erlca_util:make_dir(Dir),

    %% we stream attachments.
    {ok, Fd} = file:open(Path, [write]),
    {ok, Ref} = couchbeam:stream_fetch_attachment(Db, DocId, AttName1,
        self()),
    wait_for_attachment(Ref, Fd, AttName1, Rest, Db, DocId, AttDir).


wait_for_attachment(Ref, Fd, AttName, Rest, Db, DocId, AttDir) ->
    receive
        {Ref, done} ->
            file:close(Fd),
            attachments_to_fs(Rest, Db, DocId, AttDir);
        {Ref, {error, Error}} ->
            ?ERROR("error fetching '~p' [~p]~n", [AttName, Error]),
            attachments_to_fs(Rest, Db, DocId, AttDir);
        {Ref, {ok, Bin}} ->
            file:write(Fd, Bin),
            wait_for_attachment(Ref, Fd, AttName, Rest, Db, DocId, AttDir)
    end.

doc_to_fs([], _Dir, _Manifest, _Objects, _Depth) ->
    ok;
doc_to_fs([{<<"couchapp">>, _}|Rest], Dir, Manifest, Objects,
        Depth) when Depth < 1 ->
    doc_to_fs(Rest, Dir, Manifest, Objects, Depth);
doc_to_fs([{<<"_rev">>, _}|Rest], Dir, Manifest, Objects,
        Depth) when Depth < 1 ->
    doc_to_fs(Rest, Dir, Manifest, Objects, Depth);
doc_to_fs([{<<"_attachments">>, _}|Rest], Dir, Manifest, Objects,
        Depth) when Depth < 1 ->
    doc_to_fs(Rest, Dir, Manifest, Objects, Depth);
doc_to_fs([{PropName, Value}|Rest], Dir, Manifest, Objects, Depth) ->
    Path = filename:join(Dir, binary_to_list(PropName)),
    Dir1 = filename:dirname(Path),
    ok = erlca_util:make_dir(Dir1),
    case Value of
        {[_|_]} ->
            case proplists:get_value(Path, Manifest) of
                ".json"=Ext ->
                    JsonValue = ejson:encode(Value),
                    file:write_file(Path ++ Ext, JsonValue),
                    doc_to_fs(Rest, Dir, Manifest, Objects, Depth);
                _ ->
                    filelib:ensure_dir(Path),
                    {Value1} = Value,
                    ok = doc_to_fs(Value1, Path, Manifest, Objects,
                        Depth + 1),
                    doc_to_fs(Rest, Dir, Manifest, Objects, Depth)
            end;
        V when is_binary(V) ->
            {Path1, Value1} = case proplists:get_value(Path, Manifest) of
                undefined ->
                    {Path, V};
                Ext when Ext =:= ".js" ->
                    SourceId = list_to_binary(
                        erlca_macros:get_source_id(Value)),
                    V1 = case proplists:get_value(SourceId,
                            Objects) of
                        undefined ->
                            Value;
                        Encoded ->
                            base64:decode(Encoded)
                    end,
                    {Path ++ Ext, V1};
                Ext ->
                    {Path ++ Ext, V}
            end,
            file:write_file(Path1, Value1),
            doc_to_fs(Rest, Dir, Manifest, Objects, Depth);
        _ ->
            JsonValue = ejson:encode(Value),

            Path1 = case proplists:get_value(Path, Manifest) of
                undefined ->
                    Path;
                Ext ->
                    Path ++ Ext
            end,
            file:write_file(Path1, JsonValue),
            doc_to_fs(Rest, Dir, Manifest, Objects, Depth)
    end.
