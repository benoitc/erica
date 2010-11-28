%%% -*- erlang -*-
%%%
%%% This file is part of couchapp released under the Apache 2 license. 
%%% See the NOTICE for more information.

-record(config, { dir,
                  opts,
                  dbs,
                  hooks,
                  extensions }).

-record(global_state, { working_dir }).

-define(FAIL, throw({error, failed})).

-define(ABORT(Str, Args), couchapp_util:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), couchapp_log:log(debug, Str, Args)).
-define(INFO(Str, Args), couchapp_log:log(info, Str, Args)).
-define(WARN(Str, Args), couchapp_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), couchapp_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).


-record(push_options, {
        force=true,
        atomic=true,
        is_ddoc=true
}).

-record(couchapp, {
        path,
        docid,
        doc,
        old_doc=nil,
        manifest=[],
        attachments=[],
        signatures=[]
}).
