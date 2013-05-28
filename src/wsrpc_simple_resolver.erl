%%%-------------------------------------------------------------------
%%% @author vjache <vjache@zen>
%%% @copyright (C) 2013, vjache
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2013 by vjache <vjache@zen>
%%%-------------------------------------------------------------------
-module(wsrpc_simple_resolver).

%% API
-export([resolve/1, handle_rpc_call/1]).

resolve(_ServicePath) ->
    {mfa, ?MODULE, handle_rpc_call, []}.

handle_rpc_call(Data) ->
    Data.
