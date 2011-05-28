%%% -*- erlang -*-
%%%
%%% This file is part of erlca released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(erlca).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-export([main/1]).

main(Args) ->
    case catch(erlca_core:run(Args)) of
        ok ->
            ok;
        {error, failed} ->
            halt(1);
        Error ->
            io:format("Uncaught error in erlca: ~p\n", [Error]),
            halt(1)
    end.
