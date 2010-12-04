%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couchapp_push).

-author('Benoît Chesneau <benoitc@e-engura.org>').

-include_lib("kernel/include/file.hrl").
-include("couchapp.hrl").
-include("deps/couchbeam/include/couchbeam.hrl").

-export([push/2]).

-export([make_doc/1, couchapp_from_fs/1, process_signatures/1]).

%% ====================================================================
%% Public API
%% ====================================================================

push([DbString], Config) ->
    push1(couchapp_util:get_cwd(), DbString, Config);
push([Path, DbString|_], Config) ->
    push1(Path, DbString, Config).

push1(Path, DbString, Config) ->
    Path1 = filename:absname(Path),
    case couchapp_util:in_couchapp(Path1) of
        {ok, CouchappDir} ->
            Db = couchapp_util:db_from_config(Config, DbString),
            ?DEBUG("push ~p to ~p~n", [CouchappDir, DbString]),
            do_push(CouchappDir, Db, Config); 

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
            couchbeam:save_doc(Db, Doc);
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

    ?DEBUG("removed attachments: ~p~n", [Removed]),
    ?DEBUG("new attachments ~p~n", [NewAtts]),

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
    process_macros(Doc2, AppDir).

process_macros(Doc, AppDir) ->
    Funs = [<<"shows">>, <<"lists">>, <<"updates">>, <<"filters">>, 
        <<"spatial">>, <<"validate_update_doc">>],
    %% apply macros too functions
    {Doc1, Objects} = process_macros_fun(Funs, Doc, [], Doc, AppDir),
    {Doc2, FinalObjects} = case couchbeam_doc:get_value(<<"views">>, Doc1) of
        undefined ->
            {Doc1, Objects};
        {Views} -> 
            %% process macros in views
            {Views1, ViewObjects} = lists:mapfoldl(fun({ViewName, View}, Acc) ->
                        {View1, Objects1} = process_macros_fun([<<"map">>, <<"reduce">>], 
                            View, [], Doc1, AppDir),
                        {{ViewName, View1}, Acc ++ Objects1}
                        end, [], Views),
            {couchbeam_doc:set_value(<<"views">>, {Views1}, Doc1),
                Objects ++ ViewObjects}
    end,
    Meta = couchbeam_doc:get_value(<<"couchapp">>, Doc2),
    Meta1 = couchbeam_doc:set_value(<<"objects">>, FinalObjects, Meta),

    couchbeam_doc:set_value(<<"couchapp">>, Meta1, Doc2).

%% ===================================================================
%% Internal functions
%% ===================================================================

process_macros_fun([], Obj, Objects, _Doc, _AppDir) ->
    {Obj, Objects};
process_macros_fun([Prop|Rest], Obj, Objects, Doc, AppDir) ->
    case couchbeam_doc:get_value(Prop, Obj) of
        undefined ->
            process_macros_fun(Rest, Obj, Objects, Doc, AppDir);
        Source when is_binary(Source) ->
            ?DEBUG("process function ~p~n", [Prop]),
            Source1 = apply_macros(Source, Doc, AppDir),
            Obj1 = couchbeam_doc:set_value(Prop, Source1, Obj),
            Objects1 = if Source =/= Source1 -> 
                    SourceId = lists:flatten([io_lib:format("~.16b",[N]) 
                            || N <-binary_to_list(crypto:md5(Source1))]),
                    [{SourceId, base64:encode(Source)}|Objects];
                true ->
                    Objects
            end,

            process_macros_fun(Rest, Obj1, Objects1, Doc, AppDir);
        {Sources} ->
            ?DEBUG("process function ~p ~n", [Prop]),
            Sources1 = lists:foldl(fun({Fun1, Source}, Acc) ->
                    Source1 = apply_macros(Source, Doc, AppDir),
                    Parsed = if Source =/= Source1 ->
                            SourceId = lists:flatten([io_lib:format("~.16b",[N]) 
                                || N <-binary_to_list(crypto:md5(Source1))]),
                            {Fun1, Source1, SourceId,
                                base64:encode(Source)};
                        true ->
                            {Fun1, Source1}
                    end,

                    [Parsed|Acc]
                end, [], Sources),

            {FinalsSources, Objects1} = lists:mapfoldl(fun(Parsed, Acc) ->
                        case Parsed of
                            {Fun1, Source1, SourceId, Source} ->
                                {{Fun1, Source1}, [{SourceId,
                                            Source}|Acc]};
                            {Fun1, Source1} ->
                                {{Fun1, Source1}, Acc}
                        end
                        end, Objects, Sources1),
            Obj1 = couchbeam_doc:set_value(Prop, {FinalsSources}, Obj),
            process_macros_fun(Rest, Obj1, Objects1, Doc, AppDir)
    end.

apply_macros(Source, Doc, AppDir) ->
    Source1 = apply_code_macros(Source, AppDir),
    apply_json_macros(Source1, Doc, AppDir).

apply_code_macros(Source, AppDir) ->
    case re:run(Source, "^\s*\/\/\ ?!code (.*)$", 
            [global, caseless, unicode, multiline, 
                {capture, all, binary}]) of
        nomatch -> Source;
        {match, Matches} ->
            apply_code_macros1(Matches, Source, AppDir)
    end.

apply_code_macros1([], Source, _AppDir) ->
    Source;
apply_code_macros1([Match|Rest], Source, AppDir) ->
    [Replacement, Path] = Match,
    Path1 = filename:join(AppDir, binary_to_list(Path)),
    Contents = lists:foldl(fun(File, Acc) ->
                case file:read_file(File) of
                    {ok, Bin} ->
                        Bin1 = apply_code_macros(Bin, AppDir),
                        [Bin1|Acc];
                    Error ->
                        ?ERROR("macro: can't read ~p, [~p]~n", [File,
                                Error]),
                        Acc
                end
        end, [], filelib:wildcard(Path1)), 
    Content = iolist_to_binary(lists:reverse(Contents)),
    Source1 = re:replace(Source, Replacement, Content, [global, caseless, 
            multiline, {return, binary}]),
    apply_code_macros1(Rest, Source1, AppDir).

apply_json_macros(Source, Doc, AppDir) ->
    case re:run(Source, "^\s*\/\/\ ?!json (.*)$", 
            [global, caseless, unicode, multiline, 
                {capture, all, binary}]) of
        nomatch -> Source;
        {match, Matches} ->
            apply_json_macros1(Matches, Source, Doc, AppDir)
    end.

apply_json_macros1([], Source, _Doc, _AppDir) ->
    Source;
apply_json_macros1([Match|Rest], Source, Doc, AppDir) ->
    [Replacement, JsonPath] = Match,
    Content = case JsonPath of
        <<"_attachments", _/binary>> ->
            AttDir = filename:join(AppDir, "_attachments"),
            JsonPath1 = binary_to_list(JsonPath),
            Path = filename:join(AppDir, JsonPath1),
            Values = obj_from_dir(filelib:wildcard(Path), AttDir, []),
            lists:flatten(["var _attachments = ", 
                    couchbeam_util:json_encode({Values}), ";\n"]);
        _ ->
            Props = [list_to_binary(P) || P <- string:tokens(binary_to_list(JsonPath), 
                    ".")],
            case get_value(Props, Doc) of
                {ok, Value} ->
                    [VarName|Fields] = Props,
                    Value1 = nested_value(Fields, Value),
                    lists:flatten(["var ", VarName, " = ",
                            couchbeam_util:json_encode(Value1), ";\n"]);
                _Error ->
                    ?ERROR("json macro: can't find ~p~n", [JsonPath]),
                    ""
            end
    end,
    Source1 = re:replace(Source, Replacement, Content, [global, caseless, 
            multiline, {return, binary}]),
    apply_json_macros1(Rest, Source1, Doc, AppDir).

obj_from_dir([], _RootDir,  Att) ->
    Att;
obj_from_dir([File|Rest], RootDir, Att) ->
    File1 = filename:join(RootDir, File),
    Att1 = case filelib:is_dir(File) of
        true ->
            Value = obj_from_dir(filelib:wildcard("*", File), File, []),
            [VarName|_] = lists:reverse(string:tokens(File, "/")),
            [{VarName, Value}|Att];
        false ->
            case file:read_file(File1) of 
                {ok, Bin} ->
                    Bin1 = case filename:extension(File) of
                        ".json" ->
                            couchbeam:json_decode(Bin);
                        _ ->
                            Bin 
                    end,
                    [{filename:basename(File), Bin1}|Att];
                Error ->
                    ?ERROR("macro: can't read ~p, [~p]~n", [File,
                        Error]),
                    Att
            end
    end,
    obj_from_dir(Rest, RootDir, Att1).

nested_value(Fields, Value) ->
    case Fields of
        [] ->
            Value;
        _ ->
            nested_value1(Fields, Value, length(Fields), 1)
    end.

nested_value1([Field|Rest], Value, Len, Count) when Count < length(Rest) ->
    couchbeam_doc:set_value(Field, 
        nested_value1(Rest, Value, Len, Count+1), {[]});
nested_value1([Field|_], Value, _Len, _Count) ->
    {[{Field, Value}]}.


get_value(Props, Doc) ->
    get_value(Props, Doc, length(Props), 1).

get_value([Name|Rest], Obj, Len, Count) when Count < Len ->
    case couchbeam_doc:get_value(Name, Obj) of
        undefined ->
            not_found;
        {[_]}=Value ->
            get_value(Rest, Value, Len, Count + 1);
        _ ->
            not_found
    end;
get_value([Name|_], Obj, _Len, _Count) ->
   case couchbeam_doc:get_value(Name, Obj) of
       undefined ->
           not_found;
       Value ->
           {ok, Value}
    end.

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
    Doc1 = couchbeam_attachments:add_inline(Doc, Content, RelPath),
    attach_files(Rest, Doc1, AttDir).

process_path([], _Dir, Couchapp) ->
    {ok, Couchapp};
process_path([".couchapprc"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([".couchappignore"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([File|Rest], Dir, #couchapp{path=Path, doc=Doc, 
        manifest=Manifest}=Couchapp) ->
    Fname = filename:join(Dir, File),
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
                    {SubDoc, SubManifest} = process_dir(Files, Fname,
                        Path, {[]}, []),
                    Doc1 = couchbeam_doc:set_value(File1, SubDoc, Doc),
                    Manifest1 = [<<RelPath/binary, "/">>|Manifest] ++ SubManifest,
                    Couchapp#couchapp{doc=Doc1, manifest=Manifest1}
            end;
        false ->
            {PropName, Value} = process_file(File1, Fname),
            Doc1 = couchbeam_doc:set_value(PropName, Value, Doc),
            Couchapp#couchapp{doc=Doc1, manifest=[RelPath|Manifest]}

    end,
    process_path(Rest, Dir, Couchapp1).

process_dir([], _Dir, _Path, Doc, Manifest) ->
    {Doc, Manifest};
process_dir([File|Rest], Dir, Path, Doc, Manifest) ->
    Fname = filename:join(Dir, File),
    File1 = list_to_binary(File),
    RelPath = list_to_binary(couchapp_util:relpath(Fname, Path)),
    {Doc1, Manifest1} = case filelib:is_dir(Fname) of
        true ->
            Files = filelib:wildcard("*", Fname),
            {SubDoc, SubManifest} = process_dir(Files, Fname, Path, {[]}, []),
            NewDoc = couchbeam_doc:set_value(File1, SubDoc, Doc),
            NewManifest = [<<RelPath/binary, "/">>|Manifest] ++ SubManifest,
            {NewDoc, NewManifest};
        false ->
            {PropName, Value} = process_file(File1, Fname),
            NewDoc = couchbeam_doc:set_value(PropName, Value, Doc),
            {NewDoc, [RelPath|Manifest]}
    end,
    process_dir(Rest, Dir, Path, Doc1, Manifest1).

process_file(File, Fname) ->
    case filename:extension(Fname) of
        [] ->
            {ok, Value} = file:read_file(Fname),
            {list_to_binary(File), Value};
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
attachments_from_fs1([F|R], Dir, Att) ->
    F1 = filename:join(Dir, F),
    Att1 = case filelib:is_dir(F1) of
        true ->
            Files = filelib:wildcard("*", F1),
            SubAtt = attachments_from_fs1(Files, F1, []),
            Att ++ SubAtt;
        false ->
            {ok, Md5} = couchapp_util:md5_file(F1),
            Md5Hash = lists:flatten([io_lib:format("~.16b",[N]) 
                    || N <-binary_to_list(Md5)]),
            [{F1, list_to_binary(Md5Hash)}|Att]
    end,
    attachments_from_fs1(R, Dir, Att1).
