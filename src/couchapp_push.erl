%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(couchapp_push).

-author('Benoît Chesneau <benoitc@e-engura.org>').

-include_lib("kernel/include/file.hrl").
-include("couchapp.hrl").
-include("couchbeam.hrl").

-export([push/2]).

-export([make_doc/1, couchapp_from_fs/1, process_signatures/1]).

%% ====================================================================
%% Public API
%% ====================================================================


push([], Config) ->
    push(["default"], Config);
push([DbKey], Config) ->
    push1(couchapp_util:get_cwd(), DbKey, Config);

push([Path, DbKey|_], Config) ->
    push1(Path, DbKey, Config).

push1(Path, DbKey, Config) ->
    Path1 = filename:absname(Path),
    case couchapp_util:in_couchapp(Path1) of
        {ok, CouchappDir} ->
            %% load app conf from .couchapprc and initialize ignore
            %% patterns.
            Config1 = couchapp_config:update(CouchappDir, Config),

            Db = couchapp_util:db_from_key(Config1, DbKey),
            ?DEBUG("push ~p to ~p~n", [DbKey, CouchappDir]),
            do_push(CouchappDir, Db, Config1);

        {error, not_found} ->
            ?ERROR("Can't find initialized couchapp in '~p'~n", [Path]),
            halt(1)
    end.

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

    Couchapp = #couchapp{
        config=Config,
        path=Path,
        att_dir=filename:join(Path, "_attachments"),
        docid=DocId,
        doc={[{<<"_id">>, DocId}]},
        old_doc = OldDoc
    },

    {ok, Couchapp1} = couchapp_from_fs(Couchapp),

    %% clean attachments and process signatures
    Couchapp2 = process_signatures(Couchapp1),

    case couchapp_config:get(Config, atomic) of
        true ->
            FinalCouchapp = process_attachments(Couchapp2),
            Doc = make_doc(FinalCouchapp),
            {ok, _} = couchbeam:save_doc(Db, Doc);
        false ->
            Doc = make_doc(Couchapp2),
            Doc1 = couchbeam:save_doc(Db, Doc),
            send_attachments(Db, Couchapp2#couchapp{doc=Doc1})
    end,
    CouchappUrl = couchbeam:make_url(Server, couchbeam:doc_url(Db,
            DocId), []),

    % log info
    couchapp_log:log(info, "~p has been pushed from ~s.~n", [CouchappUrl,
            Path]),
    ok.

id_from_path(Path, Config) ->
    IdFile = filename:join(Path, "_id"),
    case filelib:is_regular(IdFile) of
        true ->
            {ok, Bin} = file:read_file(IdFile),
            Bin;
        false ->
            case couchapp_config:get(Config, docid) of
                undefined ->
                    Fname = list_to_binary(filename:basename(Path)),
                    case couchapp_config:get(Config, is_ddoc) of
                        true ->
                            <<"_design/", Fname/binary>>;
                        false ->
                            Fname
                    end;
                DocId ->
                    DocId
            end
    end.

couchapp_from_fs(#couchapp{path=Path}=Couchapp) ->
    Files = filelib:wildcard("*", Path),
    process_path(Files, Path, Couchapp).

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

    NewSignatures = [{couchapp_util:relpath(F, AttDir), S}
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

    %% set manifest an signatures in couchapp object
    Doc2 = case couchbeam_doc:get_value(<<"couchapp">>, Doc1) of
        undefined ->
            couchbeam_doc:set_value(<<"couchapp">>, {[
                        {<<"manifest">>, Manifest},
                        {<<"signatures">>, {Signatures}}
            ]}, Doc1);
        Meta ->
            Meta1 = couchbeam_doc:set_value(<<"manifest">>, Manifest, Meta),
            FinalMeta = couchbeam_doc:set_value(<<"signatures">>,
                Signatures, Meta1),
            couchbeam_doc:set_value(<<"couchapp">>, FinalMeta, Doc1)
    end,
    couchapp_macros:process_macros(Doc2, AppDir).

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
    RelPath = couchapp_util:relpath(Fname, AttDir),
    {ok, Doc1} = couchbeam:put_attachment(Db, couchbeam_doc:get_id(Doc),
        RelPath, Fun, Params),
    send_attachments1(Rest, Doc1, Db, AttDir).

attach_files([], Doc, _AttDir) ->
    Doc;
attach_files([{Fname, _Signature}|Rest], Doc, AttDir) ->
    {ok, Content} = file:read_file(Fname),
    RelPath = couchapp_util:relpath(Fname, AttDir),
    Doc1 = couchbeam_attachments:add_inline(Doc, Content,
        encode_path(RelPath)),
    attach_files(Rest, Doc1, AttDir).

process_path([], _Dir, Couchapp) ->
    {ok, Couchapp};
process_path([".couchapprc"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([".couchappignore"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([File|Rest], Dir, #couchapp{config=Config, path=Path,
        doc=Doc, manifest=Manifest}=Couchapp) ->
    Fname = filename:join(Dir, File),
    case couchapp_ignore:ignore(couchapp_util:relpath(Fname, Path), Config) of
        true ->
            process_path(Rest, Dir, Couchapp);
        false ->
            File1 = list_to_binary(File),
            RelPath = list_to_binary(couchapp_util:relpath(Fname, Path)),
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
    case couchapp_ignore:ignore(couchapp_util:relpath(Fname, Path), Config) of
        true ->
            process_dir(Rest, Dir, Path, Config, Doc, Manifest);
        false ->
            File1 = list_to_binary(File),

            RelPath = list_to_binary(couchapp_util:relpath(Fname, Path)),
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

process_file(File, Fname) ->
    case filename:extension(Fname) of
        [] ->
            {ok, Value} = file:read_file(Fname),
            {list_to_binary(File), Value};
        Ext ->
            {ok, Bin} = file:read_file(Fname),
            Value = case Ext of
                ".json" ->
                    ejson:decode(Bin);
                _ ->
                    Bin
            end,
            PropName = filename:basename(Fname, Ext),
            {list_to_binary(PropName), Value}
    end.

attachments_from_fs(#couchapp{path=Path}=Couchapp) ->
    AttPath = filename:join(Path, "_attachments"),
    Files = filelib:wildcard("*", AttPath),
    Attachments = attachments_from_fs1(Files, AttPath, Couchapp, []),
    Couchapp#couchapp{attachments=Attachments}.

attachments_from_fs1([], _Dir, _Couchapp, Att) ->
    Att;
attachments_from_fs1([F|R], Dir, #couchapp{path=Root, config=Conf}=Couchapp, Att) ->
    Path = filename:join(Dir, F),
    case couchapp_ignore:ignore(couchapp_util:relpath(Path, Root), Conf) of
        true ->
            attachments_from_fs1(R, Dir, Couchapp, Att);
        false ->
            Att1 = case filelib:is_dir(Path) of
                       true ->
                           Files = filelib:wildcard("*", Path),
                           SubAtt = attachments_from_fs1(Files, Path, Couchapp, []),
                           Att ++ SubAtt;
                       false ->
                           {ok, Md5} = couchapp_util:md5_file(Path),
                           Md5Hash = lists:flatten([io_lib:format("~.16b",[N])
                                                    || N <-binary_to_list(Md5)]),
                           [{Path, list_to_binary(Md5Hash)}|Att]
                   end,
            attachments_from_fs1(R, Dir, Couchapp, Att1)
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
