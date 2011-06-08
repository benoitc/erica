%%% -*- erlang -*-
%%%
%%% This file is part of erica released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erica_macros).

-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("erica.hrl").

-export([process_macros/2,
         get_source_id/1]).

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
    Meta1 = couchbeam_doc:set_value(<<"objects">>, {FinalObjects}, Meta),

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
                    SourceId = get_source_id(Source1),
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
                            SourceId = get_source_id(Source1),
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
                    ejson:encode({Values}), ";\n"]);
        _ ->
            Props = [list_to_binary(P) || P <- string:tokens(binary_to_list(JsonPath),
                    ".")],
            case get_value(Props, Doc) of
                {ok, Value} ->
                    [VarName|Fields] = Props,
                    Value1 = nested_value(Fields, Value),
                    lists:flatten(["var ", VarName, " = ",
                            ejson:encode(Value1), ";\n"]);
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

get_source_id(Source) ->
    lists:flatten([io_lib:format("~.16b",[N])
            || N <-binary_to_list(crypto:md5(Source))]).
