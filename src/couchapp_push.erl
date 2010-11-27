%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couchapp_push).

-author('Benoît Chesneau <benoitc@e-engura.org>').

-include_lib("kernel/include/file.hrl").
-include("couchapp.hrl").
-include("deps/couchbeam/include/couchbeam.hrl").

-export([push/2, push/3, push/4]).

-export([make_doc/1, couchapp_from_fs/1, process_signatures/1]).

push(Path, Db) ->
    push(Path, Db, #push_options{}).

push(Path, Db, Options) ->
    DocId = id_from_path(Path, Options),
    push(Path, Db, DocId, Options).

push(Path, #db{server=Server}=Db, DocId, 
        #push_options{atomic=Atomic}=Options) ->

    OldDoc = case couchbeam:open_doc(Db, DocId) of
        {ok, OldDoc1} ->
            OldDoc1;
        {error, not_found} -> 
            nil
    end,

    Couchapp = #couchapp{
        path=Path,
        docid=DocId,
        doc={[{<<"_id">>, DocId}]},
        old_doc = OldDoc
    },

    {ok, Couchapp1} = couchapp_from_fs(Couchapp),
    case Atomic of
        true ->
            Couchapp2 = process_signatures(Couchapp1),
            FinalCouchapp = process_attachments(Couchapp2),
            #couchapp{doc=Doc} = FinalCouchapp,
            Doc = make_doc(FinalCouchapp),
            couchbeam:save_doc(Db, Doc);
        false ->
            Doc = make_doc(Couchapp1),
            Doc1 = couchbeam:save_doc(Db, Doc),
            send_attachments(Db, Couchapp1#couchapp{doc=Doc1})
    end,
    CouchappUrl = couchbeam:make_url(Server, couchbeam:doc_url(Db,
            DocId), []),
    
    % log info
    couchapp_log:log(info, "~p has been pushed from ~s.~n", [CouchappUrl,
            Path]),
    ok.


id_from_path(Path, #push_options{is_ddoc=IsDdoc}) ->
    IdFile = filename:join(Path, "_id"),
    case filelib:is_regular(IdFile) of
        true ->
            {ok, Bin} = file:read_file(IdFile),
            Bin;
        false ->
            Fname = list_to_binary(filename:basename(Path)),
            case IsDdoc of
                true ->
                    <<"_design/", Fname/binary>>;
                false ->
                    Fname
            end
    end.
        

couchapp_from_fs(#couchapp{path=Path}=Couchapp) ->
    Files = filelib:wildcard("*", Path),
    process_path(Files, Path, Couchapp).

process_signatures(#couchapp{old_doc=nil}=Couchapp) ->
    Couchapp;
process_signatures(#couchapp{doc=Doc, old_doc=OldDoc,
        attachments=Atts}=Couchapp) ->
    case couchbeam_doc:get_value(<<"couchapp">>, OldDoc) of
        undefined ->
            %% oups it seems that old doc wasn't created
            %% with couchapp
            Couchapp;
        Meta ->
            case couchbeam_doc:get_value(<<"signatures">>, Meta) of
                undefined ->
                    %% not defined.
                    Couchapp;
                {Signatures} ->
                    {Removed, NewAtts} = process_signatures1(Signatures,
                        [], Atts),
                    NewSignatures = [S || {_Fname, S} <- NewAtts],
                    case Removed of 
                        [] ->  
                            Couchapp#couchapp{
                                attachments=NewAtts,
                                signatures=NewSignatures
                            };
                        _Else ->
                            {OldAtts} = couchbeam_doc:get_value(
                                <<"_attachments">>, OldDoc),
                            OldAtts1 = clean_old_attachments(
                                Removed, OldAtts),
                            Doc1 = couchbeam_doc:set_value(<<"_attachments">>,
                                {OldAtts1}, Doc),
                            Couchapp#couchapp{
                                doc=Doc1,
                                attachments=NewAtts,
                                signatures=NewSignatures
                            }
                    end
            end
    end.


process_attachments(#couchapp{doc=Doc, attachments=Atts}=Couchapp) ->
    NewDoc = attach_files(Atts, Doc),
    Couchapp#couchapp{doc=NewDoc}.

send_attachments(Db, #couchapp{doc=Doc, attachments=Atts}=Couchapp) ->
    NewDoc = send_attachments1(Atts, Doc, Db),
    Couchapp#couchapp{doc=NewDoc}.

make_doc(Couchapp) ->
    #couchapp{
        doc=Doc,
        old_doc=OldDoc,
        manifest=Manifest,
        signatures=Signatures} = Couchapp,

    Doc1 = if OldDoc =:= nil ->
            Doc;
        true ->
            couchbeam_doc:setvalue(<<"_rev">>,
                couchbeam_doc:get_rev(OldDoc), Doc)
    end,

    %% set manifest an signatures in couchapp object
    Doc2 = case couchbeam_doc:get_value(<<"couchapp">>, Doc1) of
        undefined ->
            couchbeam_doc:set_value(<<"couchapp">>, {[
                        {<<"manifest">>, Manifest},
                        {<<"signatures">>, Signatures}
            ]}, Doc1);
        Meta ->
            Meta1 = couchbeam_doc:set_value(<<"manifest">>, Manifest, Meta),
            FinalMeta = couchbeam_doc:set_value(<<"signatures">>,
                Signatures, Meta1),
            couchbeam_doc:set_value(<<"couchapp">>, FinalMeta, Doc1)
    end,
    process_macros(Doc2).
    


%% ===================================================================
%% Internal functions
%% ===================================================================

process_macros(Doc) ->
    %% do nothing for now.
    Doc.

clean_old_attachments([],OldAtts) ->
    OldAtts;    
clean_old_attachments([F|Rest], OldAtts) ->
    OldAtts1 = proplists:delete(F, OldAtts),
    clean_old_attachments(Rest, OldAtts1).

process_signatures1([], Removed, Attachments) ->
    {Removed, Attachments};
process_signatures1([{F, S}|Rest], Removed, Attachments) ->
    case proplists:get_value(F, Attachments) of
        undefined ->
            process_signatures1(Rest, [F|Removed], Attachments);
        S1 when S =:= S1 ->
            Attachments1 = proplists:delete(F, Attachments),
            process_signatures1(Rest, Removed, Attachments1);
        _S1 ->
            process_signatures1(Rest, [F|Removed], Attachments)
    end.

send_attachments1([], Doc, _Db) ->
    Doc;
send_attachments1([{Fname, _Signature}|Rest], Doc, Db) ->
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
    {ok, Doc1} = couchbeam:put_attachment(Db, couchbeam_doc:get_id(Doc),
        Fun, Params),
    send_attachments1(Rest, Doc1, Db).

attach_files([], Doc) ->
    Doc;
attach_files([{Fname, _Signature}|Rest], Doc) ->
    Content = file:read_file(Fname),
    Doc1 = couchbeam_attachments:add_inline(Doc, Content, Fname),
    attach_files(Rest, Doc1).

process_path([], _Dir, Couchapp) ->
    {ok, Couchapp};
process_path(["."|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([".."|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([File|Rest], Dir, #couchapp{doc=Doc, manifest=Manifest}=Couchapp) ->
    Fname = filename:join(Dir, File),
    File1 = list_to_binary(File),
    Couchapp1 = case filelib:is_dir(Fname) of
        true ->
            case File of
                <<"_attachments">> ->
                    attachments_from_fs(Couchapp);
                <<"_", _/binary>> ->
                    Couchapp;
                _ ->
                    Files = filelib:wildcard("*", Fname),
                    {SubDoc, SubManifest} = process_dir(Files, Fname, 
                        {[]}, []),
                    Doc1 = couchbeam_doc:set_value(File1, SubDoc, Doc),
                    Manifest1 = [Fname|Manifest] ++ SubManifest,
                    Couchapp#couchapp{doc=Doc1, manifest=Manifest1}
            end;
        false ->
            Content = process_file(File1, Fname),
            Doc1 = couchbeam_doc:set_value(File1, Content, Doc),
            Couchapp#couchapp{doc=Doc1, manifest=[Fname|Manifest]}

    end,
    process_path(Rest, Dir, Couchapp1).


process_dir([], _Dir, Doc, Manifest) ->
    {Doc, Manifest};
process_dir(["."|Rest], Dir, Doc, Manifest) ->
    process_dir(Rest, Dir, Doc, Manifest);
process_dir([".."|Rest], Dir, Doc, Manifest) ->
    process_dir(Rest, Dir, Doc, Manifest);
process_dir([File|Rest], Dir, Doc, Manifest) ->
    Fname = filename:join(Dir, File),
    File1 = list_to_binary(File),
    {Doc1, Manifest1} = case filelib:is_dir(Fname) of
        true ->
            Files = filelib:wildcard("*", Fname),
            {SubDoc, SubManifest} = process_dir(Files, Fname, {[]}, []),
            NewDoc = couchbeam_doc:set_value(File1, SubDoc, Doc),
            NewManifest = [Fname|Manifest] ++ SubManifest,
            {NewDoc, NewManifest};
        false ->
            {PropName, Value} = process_file(File1, Fname),
            NewDoc = couchbeam_doc:set_value(PropName, Value, Doc),
            {NewDoc, [Fname|Manifest]}
    end,
    process_dir(Rest, Dir, Doc1, Manifest1).

process_file(File, Fname) ->
    case filename:extension(Fname) of
        [] ->
            {ok, Value} = file:read_file(Fname),
            {File, Value};
        Ext ->
            {ok, Bin} = file:read_file(Fname),
            Value = case Ext of
                ".json" ->
                    couchbeam_util:json_decode(Bin);
                _ ->
                    Bin
            end,
            PropName = filename:basename(Fname, Ext),
            {list_to_binary(PropName), Value}
    end.


attachments_from_fs(#couchapp{path=Path}=Couchapp) ->
    AttPath = filename:join(Path, "_attachments"),
    Files = filelib:wildcard("*", AttPath),
    Attachments = attachments_from_fs1(Files, AttPath, []),
    Couchapp#couchapp{attachments=Attachments}.

attachments_from_fs1([], _Dir, Att) ->
    Att;
attachments_from_fs1(["."|R], Dir, Att) ->
    attachments_from_fs1(R, Dir, Att);
attachments_from_fs1([".."|R], Dir, Att) ->
    attachments_from_fs1(R, Dir, Att);
attachments_from_fs1([F|R], Dir, Att) ->
    F1 = filename:join(Dir, F),
    Att1 = case filelib:is_dir(F1) of
        true ->
            Files = filelib:wildcard("*", F1),
            SubAtt = attachments_from_fs1(Files, F1, []),
            Att ++ SubAtt;
        false ->
            [{F1, couchap_util:md5_file(F1)}|Att]
    end,
    attachments_from_fs1(R, Dir, Att1).
