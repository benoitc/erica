%%% -*- erlang -*-
%%%
%%% This file is part of erica released under the Apache 2 license. 
%%% See the NOTICE for more information.

-record(config, { dir,
                  opts,
                  dbs=[],
                  hooks=[],
                  ignore=[],
                  extensions=[] }).

-record(global_state, { working_dir }).

-define(FAIL, throw({error, failed})).

-define(ABORT(Str, Args), erica_util:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), erica_log:log(debug, Str, Args)).
-define(INFO(Str, Args), erica_log:log(info, Str, Args)).
-define(WARN(Str, Args), erica_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), erica_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-record(couchapp, {
        config,
        path,
        ddoc_dir,
        att_dir,
        docid,
        doc,
        pushed_by,
        old_doc=nil,
        manifest=[],
        attachments=[],
        signatures=[]
}).
