%%% -*- erlang -*-
%%%
%%% This file is part of erica released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erica_push).

-author('Benoît Chesneau <benoitc@e-engura.org>').

-include_lib("kernel/include/file.hrl").
-include_lib("erica/include/erica.hrl").
-include_lib("couchbeam/include/couchbeam.hrl").

-export([push/2]).

-export([make_doc/1, couchapp_from_fs/1, process_signatures/1,
        id_from_path/2, index_url/2, do_push/3, do_push/4]).

%% ====================================================================
%% Public API
%% ====================================================================


push([], Config) ->
    push(["default"], Config);
push([DbKey], Config) ->
    push1(erica_util:get_cwd(), DbKey, Config);

push([Path, DbKey|_], Config) ->
    push1(Path, DbKey, Config).

push1(".", DbKey, Config) ->
    push1("./", DbKey, Config);

push1(Path, DbKey, Config) ->
    Path1 = filename:absname(Path),
    case erica_util:in_couchapp(Path1) of
        {ok, CouchappDir} ->
            push2(CouchappDir, DbKey, Config),
            ok;

        {error, not_found} ->
            push2(Path1, DbKey, Config),
            ok
    end.

push2(CouchappDir, DbKey, Config) ->
    Config1 = erica_config:update(CouchappDir, Config),

    Db = erica_util:db_from_key(Config1, DbKey),
    ?DEBUG("push ~s => ~s~n", [CouchappDir, DbKey]),
    {ok, _} = do_push(CouchappDir, Db, Config1).


do_push(Path, Db, Config) ->
    DocId = id_from_path(Path, Config),
    do_push(Path, Db, DocId, Config).

do_push(Path, #db{server=Server}=Db, DocId, Config) ->
    OldDoc = case couchbeam:open_doc(Db, DocId) of
        {ok, OldDoc1} ->
            OldDoc1;
        {error, not_found} ->
            {[]}
    end,

    RootFiles = filelib:wildcard("*", Path),
    Detected_style = detect_style(RootFiles),
    ?DEBUG("Detected Style: ~p ~n", [Detected_style]),
    Couchapp = #couchapp{
        config=Config,
        path=Path,
        pushed_by=pushed_by(Db),
        ddoc_dir=choose_ddoc_dir(Detected_style, Path),
        att_dir=choose_attach_dir(Detected_style, Path),
        docid=DocId,
        doc={[{<<"_id">>, DocId}]},
        old_doc = OldDoc
    },

    {ok, Couchapp1} = couchapp_from_fs(Couchapp),

    %% clean attachments and process signatures
    Couchapp2 = process_signatures(Couchapp1),

    case erica_config:get(Config, atomic) of
        true ->
            FinalCouchapp = process_attachments(Couchapp2),
            Doc = make_doc(FinalCouchapp),
            {ok, _} = couchbeam:save_doc(Db, Doc);
        false ->
            Doc = make_doc(Couchapp2),
            Doc1 = couchbeam:save_doc(Db, Doc),
            send_attachments(Db, Couchapp2#couchapp{doc=Doc1})
    end,

    send_docs(Couchapp2, Db ),

    CouchappUrl = couchbeam:make_url(Server, couchbeam:doc_url(Db, DocId), []),

    DisplayUrl = index_url(CouchappUrl, Couchapp1),

    ?CONSOLE("==> Successfully pushed. You can browse it at: ~s~n", [DisplayUrl]),

    % log info
    erica_log:log(info, "~p has been pushed from ~s.~n", [CouchappUrl, Path]),
    {ok, DisplayUrl}.

detect_style([]) ->
    webstyle;
detect_style(["_attachments"|_]) ->
    traditional;
detect_style(["_ddoc"|_])  ->
    webstyle;
detect_style(["index.html"|_]) ->
    webstyle;
detect_style(["_id"|_]) ->
    traditional;
detect_style(["views"|_]) ->
    traditional;
detect_style([_|Rest]) ->
    detect_style(Rest).


choose_ddoc_dir(Detected_style, Path) ->
    case erica_config:get_global(webstyle, "0") of
        "1" -> filename:join(Path, "_ddoc");
         _  -> choose_ddoc_dir1(Detected_style, Path)
    end.

choose_ddoc_dir1(webstyle, Path) ->
    filename:join(Path, "_ddoc");
choose_ddoc_dir1(_, Path) ->
    Path.

choose_attach_dir(Detected_style, Path) ->
    case erica_config:get_global(webstyle, "0") of
        "1" -> Path;
         _  -> choose_attach_dir1(Detected_style, Path)
     end.

choose_attach_dir1(webstyle, Path)
    ->  Path;
choose_attach_dir1(_, Path)
    ->  filename:join(Path, "_attachments").

index_url(CouchappUrl, #couchapp{doc=Doc}=Couchapp) ->
    CouchappObj = couchbeam_doc:get_value(<<"couchapp">>, Doc, {[]}),
    HasRewrites = case couchbeam_doc:get_value(<<"rewrites">>, Doc, []) of
        [] -> false;
        _ -> true
    end,

    FinalIndex = case couchbeam_doc:get_value(<<"index">>, CouchappObj) of
        undefined ->
             index_url2(has_index_file(Couchapp), HasRewrites);
        Index ->
            Index
    end,
    CouchappUrl ++ FinalIndex.

index_url2(true, true) -> "/_rewrite/";
index_url2(true, false) -> "/index.html";
index_url2(false, true) -> "/_rewrite/";
index_url2(_, _) -> "".

has_index_file(#couchapp{attachments=List}) ->
   lists:any(fun({File, _}) ->
       filename:basename(File) =:= "index.html"
   end, List).

id_from_path(Path, Config) ->
    IdFile = filename:join(Path, "_id"),
    case filelib:is_regular(IdFile) of
        true ->
            {ok, Bin} = file:read_file(IdFile),
            [Id|_] = binary:split(Bin, <<"\n">>, [trim]),
            Id;
        false ->
            case erica_config:get(Config, docid) of
                undefined ->
                    Fname = list_to_binary(filename:basename(Path)),
                    case erica_config:get(Config, is_ddoc) of
                        true ->
                            <<"_design/", Fname/binary>>;
                        false ->
                            Fname
                    end;
                DocId ->
                    DocId
            end
    end.

couchapp_from_fs(#couchapp{ddoc_dir=Ddoc_Path}=Couchapp) ->
    Couchapp1 = attachments_from_fs(Couchapp),
    Files1 = filelib:wildcard("*", Ddoc_Path),
    process_path(Files1, Ddoc_Path, Couchapp1).

process_signatures(#couchapp{attachments=[]}=Couchapp) ->
    Couchapp;
process_signatures(#couchapp{att_dir=AttDir, doc=Doc, old_doc=OldDoc,
        attachments=Atts}=Couchapp) ->

    Signatures = case couchbeam_doc:get_value(<<"couchapp">>, OldDoc) of
        undefined ->
            [];
        Meta ->
            case couchbeam_doc:get_value(<<"signatures">>, Meta) of
                undefined ->
                    %% not defined.
                    [];
                {Signatures1} ->
                    Signatures1
            end
    end,
    {Removed, NewAtts} = process_signatures1(Signatures, [], Atts,
        AttDir),

    NewSignatures = [{list_to_binary(erica_util:relpath(F, AttDir)), S}
        || {F, S} <- Atts],

    {OldAtts} = couchbeam_doc:get_value(<<"_attachments">>, OldDoc, {[]}),
    case Removed of
        [] ->
            Doc1 = couchbeam_doc:set_value(<<"_attachments">>,
                {OldAtts}, Doc),
            Couchapp#couchapp{
                doc=Doc1,
                attachments=NewAtts,
                signatures=NewSignatures
            };
        _Else ->
            OldAtts1 = clean_old_attachments(Removed, OldAtts),
            Doc1 = couchbeam_doc:set_value(<<"_attachments">>,
                {OldAtts1}, Doc),
            Couchapp#couchapp{
                doc=Doc1,
                attachments=NewAtts,
                signatures=NewSignatures
            }
    end.

process_attachments(#couchapp{att_dir=AttDir, doc=Doc,
        attachments=Atts}=Couchapp) ->
    NewDoc = attach_files(Atts, Doc, AttDir),
    Couchapp#couchapp{doc=NewDoc}.

send_attachments(Db, #couchapp{att_dir=AttDir, doc=Doc,
        attachments=Atts}=Couchapp) ->
    NewDoc = send_attachments1(Atts, Doc, Db, AttDir),
    Couchapp#couchapp{doc=NewDoc}.

make_doc(Couchapp) ->
    #couchapp{
        path=AppDir,
        doc=Doc,
        pushed_by=Pushed_by,
        old_doc=OldDoc,
        manifest=Manifest,
        signatures=Signatures} = Couchapp,

    Doc1 = case OldDoc of
        {[]} ->
            Doc;
        _ ->
            couchbeam_doc:set_value(<<"_rev">>,
                couchbeam_doc:get_rev(OldDoc), Doc)
    end,

    WebManifest = do_web_manifest(Couchapp),
    %Git = git_info(),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Time = list_to_binary(lists:flatten(
           	io_lib:fwrite("~4..0B-~2B-~2BT~2.10.0B:~2.10.0B:~2.10.0B",
                                 [Year, Month, Day,  Hour, Min, Sec]))),

    %% set manifest an signatures in couchapp object
    Doc2 = case couchbeam_doc:get_value(<<"couchapp">>, Doc1) of
        undefined ->
            couchbeam_doc:set_value(<<"couchapp">>, {[
                        {<<"pushed_by">>, Pushed_by},
                        {<<"push_time">>, Time},
                        {<<"build_time">>, Time},
                        {<<"manifest">>, Manifest},
                        {<<"signatures">>, {Signatures}},
                        {<<"config">>, WebManifest}
            ]}, Doc1);
        Meta ->
            Meta1 = couchbeam_doc:set_value(<<"manifest">>, Manifest, Meta),
            Meta2 = couchbeam_doc:set_value(<<"config">>, WebManifest, Meta1),
            Meta3 = couchbeam_doc:set_value(<<"pushed_by">>, Pushed_by, Meta2),
            Meta4 = couchbeam_doc:set_value(<<"push_time">>, Time, Meta3),
            Meta5 = couchbeam_doc:set_value(<<"build_time">>, Time, Meta4),
            FinalMeta = couchbeam_doc:set_value(<<"signatures">>,
                {Signatures}, Meta5),
            couchbeam_doc:set_value(<<"couchapp">>, FinalMeta, Doc1)
    end,
    erica_macros:process_macros(Doc2, AppDir).

clean_old_attachments([],OldAtts) ->
    OldAtts;
clean_old_attachments([F|Rest], OldAtts) ->
    OldAtts1 = proplists:delete(F, OldAtts),
    clean_old_attachments(Rest, OldAtts1).

process_signatures1([], Removed, Attachments, _AttDir) ->
    {Removed, Attachments};
process_signatures1([{F, S}|Rest], Removed, Attachments, AttDir) ->
    F1 = filename:join(AttDir, binary_to_list(F)),
    case proplists:get_value(F1, Attachments) of
        undefined ->
            process_signatures1(Rest, [F|Removed], Attachments, AttDir);
        S1 when S =:= S1 ->
            Attachments1 = proplists:delete(F1, Attachments),
            process_signatures1(Rest, Removed, Attachments1, AttDir);
        _S1 ->
            process_signatures1(Rest, [F|Removed], Attachments, AttDir)
    end.

send_attachments1([], Doc, _Db, _AttDir) ->
    Doc;
send_attachments1([{Fname, _Signature}|Rest], Doc, Db, AttDir) ->
    {ok, FileInfo} = file:read_file_info(Fname),
    {ok, Fd} = file:open(Fname, [read]),
    Fun = fun() ->
            case file:read(Fd, 4096) of
                {ok, Data} ->
                    {ok, iolist_to_binary(Data)};
                _ ->
                    file:close(Fd),
                    eof
            end
    end,
    Params = [
        {content_length, FileInfo#file_info.size},
        {rev, couchbeam_doc:get_rev(Doc)}
    ],
    Params2 = add_special_content_types(Params, Fname, FileInfo),
    RelPath = erica_util:relpath(Fname, AttDir),
    {ok, Doc1} = couchbeam:put_attachment(Db, couchbeam_doc:get_id(Doc),
        RelPath, Fun, Params2),
    send_attachments1(Rest, Doc1, Db, AttDir).

attach_files([], Doc, _AttDir) ->
    Doc;
attach_files([{Fname, _Signature}|Rest], Doc, AttDir) ->
    {ok, Content} = file:read_file(Fname),
    RelPath = erica_util:relpath(Fname, AttDir),
    Doc1 = case guess_mime(Fname) of
        undefined ->
           couchbeam_attachments:add_inline(Doc, Content, encode_path(RelPath));
        Mime ->
            couchbeam_attachments:add_inline(Doc, Content, encode_path(RelPath), Mime)
    end,
    attach_files(Rest, Doc1, AttDir).

add_special_content_types(Params, Fname, FileInfo) ->
    case guess_mime(Fname) of
        undefined ->
            Params;
        Mime ->
            Params ++ {content_type, Mime}
    end.

%% @spec guess_mime(string()) -> string()
%% @doc  Guess the mime type of a file by the extension of its filename.
guess_mime(File) ->
    from_extension(filename:extension(File)).

from_extension(".webapp") ->
    "application/x-web-app-manifest+json";
from_extension(_) ->
    undefined.

process_path([], _Dir, Couchapp) ->
    {ok, Couchapp};
process_path([".couchapprc"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([".ericaignore"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path(["_id"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([File|Rest], Dir, #couchapp{config=Config, ddoc_dir=Path,
        doc=Doc, manifest=Manifest}=Couchapp) ->
    Fname = filename:join(Dir, File),
    case erica_ignore:ignore(erica_util:relpath(Fname, Path), Config) of
        true ->
            process_path(Rest, Dir, Couchapp);
        false ->
            File1 = list_to_binary(File),
            RelPath = list_to_binary(erica_util:relpath(Fname, Path)),
            Couchapp1 = case filelib:is_dir(Fname) of
                true ->
                    case File1 of
                        <<"_attachments">> ->
                            attachments_from_fs(Couchapp);
                        <<"_", _/binary>> ->
                            Couchapp;
                        _ ->
                            Files = filelib:wildcard("*", Fname),
                            {SubDoc, SubManifest} = process_dir(Files,
                                Fname, Path, Config, {[]}, []),
                            Doc1 = couchbeam_doc:set_value(File1, SubDoc,
                                Doc),
                            Manifest1 = [<<RelPath/binary, "/">>|Manifest]
                                ++ SubManifest,
                            Couchapp#couchapp{doc=Doc1, manifest=Manifest1}
                    end;
                false ->
                    {PropName, Value} = process_file(File, Fname),
                    Doc1 = couchbeam_doc:set_value(PropName, Value, Doc),
                    Couchapp#couchapp{doc=Doc1, manifest=[RelPath|Manifest]}

            end,
            process_path(Rest, Dir, Couchapp1)
    end.

process_dir([], _Dir, _Path, _Config, Doc, Manifest) ->
    {Doc, Manifest};
process_dir([File|Rest], Dir, Path, Config, Doc, Manifest) ->
    Fname = filename:join(Dir, File),
    case erica_ignore:ignore(erica_util:relpath(Fname, Path), Config) of
        true ->
            process_dir(Rest, Dir, Path, Config, Doc, Manifest);
        false ->
            File1 = list_to_binary(File),

            RelPath = list_to_binary(erica_util:relpath(Fname, Path)),


            {Doc1, Manifest1} = case filelib:is_dir(Fname) of
                true ->
                    Files = filelib:wildcard("*", Fname),
                    {SubDoc, SubManifest} = process_dir(Files, Fname,
                        Path, Config, {[]}, []),
                    NewDoc = couchbeam_doc:set_value(File1, SubDoc, Doc),
                    NewManifest = [<<RelPath/binary, "/">>|Manifest]
                        ++ SubManifest,
                    {NewDoc, NewManifest};
                false ->
                    {PropName, Value} = process_file(File, Fname),
                    NewDoc = couchbeam_doc:set_value(PropName, Value,
                        Doc),
                    {NewDoc, [RelPath|Manifest]}
            end,
            process_dir(Rest, Dir, Path, Config, Doc1, Manifest1)
    end.


process_file("language", FName) ->
    {ok, Bin} = file:read_file(FName),
    [Value|_] = binary:split(Bin, <<"\n">>, [trim]),
    {<<"language">>, Value};
process_file(File, Fname) ->
    case filename:extension(Fname) of
        [] ->
            {ok, Value} = file:read_file(Fname),
            {list_to_binary(File), Value};
        Ext ->
            {ok, Bin} = file:read_file(Fname),
            Value = case Ext of
                ".json" ->
                    couchbeam_ejson:decode(Bin);
                _ ->
                    Bin
            end,
            PropName = filename:basename(Fname, Ext),
            {list_to_binary(PropName), Value}
    end.

attachments_from_fs(#couchapp{att_dir=AttPath}=Couchapp) ->
    Files = filelib:wildcard("*", AttPath),
    Attachments = attachments_from_fs1(Files, AttPath, Couchapp, []),
    Couchapp#couchapp{attachments=Attachments}.

attachments_from_fs1([], _Dir, _Couchapp, Att) ->
    Att;
attachments_from_fs1([F|R], Dir, #couchapp{path=Root, config=Conf, ddoc_dir=Ddoc_Path}=Couchapp, Att) ->
    Path = filename:join(Dir, F),
    case is_ignore_attachment(Path, Root, Ddoc_Path, Conf) of
        true ->
            attachments_from_fs1(R, Dir, Couchapp, Att);
        false ->
            Att1 = case filelib:is_dir(Path) of
                       true ->
                           Files = filelib:wildcard("*", Path),
                           SubAtt = attachments_from_fs1(Files, Path, Couchapp, []),
                           Att ++ SubAtt;
                       false ->
                           {ok, Md5} = erica_util:md5_file(Path),
                           Md5Hash = lists:flatten([io_lib:format("~.16b",[N])
                                                    || N <-binary_to_list(Md5)]),
                           [{Path, list_to_binary(Md5Hash)}|Att]
                   end,
            attachments_from_fs1(R, Dir, Couchapp, Att1)
    end.

is_ignore_attachment(Path, Root, Ddoc_Path, Conf) ->
    if Path =:= Ddoc_Path  -> true;
        true -> erica_ignore:ignore(erica_util:relpath(Path, Root), Conf)
    end.
is_utf8(S) ->
    try lists:all(fun(C) -> xmerl_ucs:is_incharset(C, 'utf-8') end, S)
    catch
        exit:{ucs, {bad_utf8_character_code}} -> false
    end.

encode_path(P) ->
    case is_utf8(P) of
        true ->
            P;
        false ->
            Parts = lists:foldl(fun(P1, Acc) ->
                    [mochiweb_util:quote_plus(P1)|Acc]
                end, [], string:tokens(P, "/")),
            string:join(lists:reverse(Parts), "/")
    end.

send_docs(#couchapp{ddoc_dir=Path}=Couchapp, Db) ->
    %This should be adjusted based on ddoc_dir
    DocPath = filename:join(Path, "_docs"),
    Files = filelib:wildcard("*", DocPath),
    {Success, Total} = docs_from_fs1(Files, {0,0},  Couchapp, Db),
    case Total of
        0 -> 0;
        _ ->
            ?CONSOLE("  > pushed ~B of ~B docs.~n", [Success, Total])
    end.


docs_from_fs1([], {Success, Total}, _, _ ) ->
    {Success, Total};
docs_from_fs1([F|R],  {Success, Total}, #couchapp{ddoc_dir=Root}=Couchapp, Db) ->
     %This should be adjusted based on ddoc_dir
    Path = filename:join(Root, '_docs'),
    DocPath = filename:join(Path, F),
    try load_doc_from_fs(DocPath) of
        Json ->
            DocPushed = push_doc(Db, Json, F),
            docs_from_fs1(R, {Success + DocPushed, Total +1}, Couchapp, Db)
    catch
        invalid_json:_ ->
            ?DEBUG("---> Failed Doc Upload ~p. Invalid JSON file. ~n", [F]),
            docs_from_fs1(R, {Success, Total + 1}, Couchapp, Db );
        _:_ ->
            ?DEBUG("---> Failed Doc Upload ~p ~n", [F]),
            docs_from_fs1(R, {Success, Total + 1}, Couchapp, Db)
    end.



load_doc_from_fs(File) ->
    {ok, Bin} = file:read_file(File),
    couchbeam_ejson:decode(Bin).


push_doc(Db, Json, F) ->
    try couchbeam:save_doc(Db, Json) of
        {ok, _} ->
            ?DEBUG("---> Doc uploaded: ~p ~n", [F]),
            1;
        {error, conflict} ->
            ?DEBUG("---> Failed Doc Upload ~p, Document Conflict ~n", [F]),
            0
    catch
        _:_ ->
            ?DEBUG("---> Failed Doc Upload ~p ~n", [F]),
            0
    end.


git_info() ->
    Test = os:cmd("git rev-list HEAD --max-count=1"),
    ?CONSOLE("---> Git info: ~p ~n", [Test]).

pushed_by(#db{options=Options}) ->
    io:format("ici", []),
    case proplists:get_value(basic_auth, Options) of
        undefined ->
            false;
        {User, _} ->
            list_to_binary(User)
    end.

do_web_manifest(#couchapp{att_dir=Path}=Couchapp) ->
    Files = filelib:wildcard("*.webapp", Path),
    do_web_manifest1(Files, Path).

do_web_manifest1([], _) ->
    none;

do_web_manifest1([F], Path) ->
    Fname = filename:join(Path, F),
    {ok, Bin} = file:read_file(Fname),
    couchbeam_ejson:decode(Bin);


do_web_manifest1([F,_], Path) ->
    Fname = filename:join(Path, F),
    {ok, Bin} = file:read_file(Fname),
    couchbeam_ejson:decode(Bin).

