-module(wsrpc_handler).

%% Behaviours
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-include("log.hrl").

-export([init/3, 
	 handle/2, 
	 terminate/3
]).

-export([websocket_init/3, 
	 websocket_handle/3,
	 websocket_info/3, 
	 websocket_terminate/3]).

-export([stream/2]).

%% WebSocket Handler state
-record(state, {service :: {gen_server, pid()} | 
			   {mfa, 
			    Mod  :: atom(), 
			    Func :: atom(), 
			    MandatoryArgs :: [] },
		streams = [],
		types_cache}).

%% Streams tracking record
-record(stream_info,   {rid, pid, mref}).

%% Protocol Messages
-record('stream-item', {rid, data}).
-record('stream-end',  {rid, data}).
-record(call, {rid, data}).

-define(t(Type), {Type, record_info(fields, Type)}).
-define(tx(Record, Props),
        {Record, [ case lists:keyfind(__F, 1, Props) of
                       false ->
			   __F;
		       __T   -> __T
                   end || __F <- record_info(fields, Record)]}).

stream({Pid, Tag} = _From, Msg) ->
    Pid ! { {stream_continue, Tag, self()}, Msg}, ok.

%%
%% Ordinary HTTP paragraph
%%

init({_Any, http}, Req, Opts) ->
    ?LOG_DEBUG([{options, Opts}]),
    DoWsUpgrade = fun() -> 
			  ?LOG_DEBUG([{ws_upgrade, Req}]),
			  {upgrade, protocol, cowboy_websocket} 
		  end,
    case cowboy_req:header(<<"upgrade">>, Req) of
	{undefined, Req2} ->
	    ?LOG_DEBUG([ordinary_http_req]),
	    {ok, Req2, undefined};
	{<<"websocket">>, _Req2} ->
	    DoWsUpgrade();
	{<<"WebSocket">>, _Req2} ->
	    DoWsUpgrade()
    end.

handle(Req, State) ->
    ?LOG_DEBUG([{http_handle, Req}]),
    {ok, Req, State}.

terminate(Reason, _Req, _State) ->
    ?LOG_DEBUG([http_terminate, 
	       {reason, Reason},
	       {state, _State}]),
     ok.

%%
%% Websocket paragraph
%%

websocket_init(_Any, Req, Opts) ->
    {ServicePath, _} = cowboy_req:path_info(Req),
    RMod    = proplists:get_value(resolver, Opts, wsrpc_gs_resolver),
    Service = RMod:resolve(ServicePath),
    case Service of
	{gen_server, Pid} -> erlang:monitor(process, Pid);
	_ -> ok
    end,
    ?LOG_DEBUG([{ws_init, ServicePath, Opts}]),
    TyCache = dict:from_list(
		[?t('call'),
		 ?t('stream-item'),
		 ?t('stream-end')]),
    {ok, 
     cowboy_req:compact(Req), 
     #state{service     = Service, 
	    types_cache = TyCache },
     hibernate}.

%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
websocket_handle({text, Json}, Req, 
		 #state{streams     = Streams, 
			types_cache = TyCache, 
			service = {gen_server, Pid}} = State) ->
    ?LOG_DEBUG([{msg_rcv, Json}]),
    Jsx = jsx:decode(Json),
    ?LOG_DEBUG([{msg_jsxed, Jsx}]),
    case jsx_util:from_jsx(Jsx, TyCache, 
			   fun(Type)-> get_type(State, Type) end) of
	{#call{rid = Rid, data = Data} = Call, 
	 TyCache1} ->
	    ?LOG_DEBUG([{msg_parsed, Call}]),
	    lists:keymember(Rid, #stream_info.rid, Streams) andalso exit(rid_clash),
	    Mref = Rid,
	    Pid ! {'$gen_call', {self(), Mref}, Data },
	    Streams1 = [#stream_info{rid = Rid} | Streams],
	    {ok, Req, State#state{streams = Streams1, types_cache = TyCache1} }
    end;
%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
websocket_handle(_Any, Req, State) ->
    ?LOG_ERROR([{unexpected_cmd, _Any}]),
    {ok, Req, State}.

%% Handle service termination
websocket_info({'DOWN', _MRef, process, Pid, Reason}, 
	       _Req, #state{service = {gen_server, Pid} } = _State) ->
    exit(Reason);
%% Handle streams termination
websocket_info({'DOWN', Mref, process, Pid, Reason}, 
	       Req, #state{streams = Streams} = State) ->
    {value, #stream_info{ pid = Pid, rid = Rid}, Streams1} = 
	lists:keytake(Mref, #stream_info.mref, Streams),
    ?LOG_DEBUG([{stream_terminated, Pid}, 
		{rid, Rid}, 
		{reason, Reason}]),
    Reply = #'stream-end'{rid = Rid, data = null},
    make_reply(Reply, Req, State#state{streams = Streams1});
%% Handle Replies
websocket_info({ Tag, Data}, 
	       Req, #state{streams = Streams} = State) ->
    case Tag of
	{stream_continue, Rid, Pid1} -> 
	    Streams1 = lists_keyupsert(
			 Rid, #stream_info.rid, Streams, 
			 fun(#stream_info{pid = Pid0} = SInfo) ->
				 if Pid0 == undefined ->
					 Mref = erlang:monitor(process, Pid1),
					 SInfo#stream_info{pid = Pid1, mref = Mref};
				    Pid0 == Pid1, is_pid(Pid0) -> 
					 SInfo
				 end
			 end),
	    Reply    = #'stream-item'{rid = Rid, data = Data};
	Rid -> 
	    {value, _, Streams1} = lists:keytake(Rid, #stream_info.rid, Streams),
	    Reply = #'stream-end'{rid = Rid, data = Data} 
    end,
    make_reply(Reply, Req, State#state{streams = Streams1}).

websocket_terminate(TermMsg, _Req, _State) ->
    case TermMsg of
	{normal, closed} -> ok;
	_Reason -> 
	    ?LOG_ERROR([{ws_terminate, _Req}, {reason, _Reason}]),
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_type(#state{service = {gen_server, Pid} }, Type) ->
    gen_server:call(Pid, {get_type, Type}).

make_reply(ReplyObj, Req, #state{types_cache = TyCache} = State) ->
    {Jsx, TyCache1} = jsx_util:to_jsx(
			ReplyObj, TyCache, 
			fun(Type)-> get_type(State, Type) end),
    {reply, {text, jsx:encode(Jsx)}, 
     Req, State#state{types_cache = TyCache1} }.

lists_keyupsert(_Key, _N, [], Fun) ->
    case Fun(undefined) of
	undefined -> [];
	NewTup when is_tuple(NewTup) ->
	    [NewTup]
    end;
lists_keyupsert(Key, N, [T | L], Fun) when element(N, T) == Key ->
    [Fun(T) | L];
lists_keyupsert(Key, N, [T | L], Fun) ->
    [T | lists_keyupsert(Key, N, L, Fun)].

 
