-module(wsrpc_handler).

%% Behaviours
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-include("log.hrl").

-export([ init/3, 
	  handle/2, 
	  terminate/3 ]).

-export([ websocket_init/3, 
	  websocket_handle/3,
	  websocket_info/3, 
	  websocket_terminate/3 ]).

-export([ stream/2 ]).

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
-define(json_error(Msg), <<"{\"type\":\"error\", \"reason\":\"",Msg,"\"}">>).

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
	    rest_init(Req2, Opts);
	{<<"websocket">>, _Req2} ->
	    DoWsUpgrade();
	{<<"WebSocket">>, _Req2} ->
	    DoWsUpgrade()
    end.

handle(Req, #state{ types_cache = TyCache, 
		    service     = {gen_server, Pid}} = State) ->
    ?LOG_DEBUG([{http_handle, Req}]),
    %% 1. Get JSON request body
    HandleJsonCall = 
	fun(ReqN, ReqJson) ->
		?LOG_DEBUG([{msg_rcv, ReqJson}]),
		{RespJson, StatusCode,  State1} = 
		    case jsx:decode(ReqJson) of
			{incomplete, _} ->
			    { ?json_error("incomplete_json"), State};
			ReqJsx ->
			    ?LOG_DEBUG([{msg_jsxed, ReqJsx}]),
			    try jsx_util:from_jsx(
				  ReqJsx, TyCache, 
				  fun(Type)-> get_type(State, Type) end) of
				{Call, TyCache1} when is_tuple(Call) ->
				    ?LOG_DEBUG([{jsx_mapped_to_record, Call}]),
				    Resp = gen_server:call( Pid, Call, infinity),
				    ?LOG_DEBUG([service_called_succesfully, 
						{call, Call}, {pid, Pid} ]),
				    make_reply(Resp, State#state{ types_cache = TyCache1});
				{_, _} ->
				    ?LOG_ERROR([ jsx_to_record_failed, 
						 {jsx, ReqJsx }]),
				    { ?json_error("json_to_record_failed"), 400, State}
			    catch
				_:Reason ->
				    ?LOG_ERROR([ jsx_to_record_failed, 
						 {reason, Reason},
						 {jsx, ReqJsx },
						 {stacktrace, erlang:get_stacktrace()}]),
				    { ?json_error("json_to_record_failed"), 400, State}
			    end
		    end,
		{ok, ReqN1} = 
		    cowboy_req:reply( 
		      StatusCode,
		      [{<<"content-encoding">>, <<"utf-8">>}, 
		       {<<"content-type">>,     <<"application/json">>}], 
		      RespJson, ReqN),
		{State1, ReqN1}
	end,		     
    {State1, ReqZ} = 
	case cowboy_req:method(Req) of
	    {<<"POST">>, Req1} -> 
		{ok, ReqJson, Req2} = cowboy_req:body(Req1),
		HandleJsonCall(Req2, ReqJson);
	    {<<"GET">>, Req1} ->
		case cowboy_req:qs_val(<<"call">>, Req1) of
		    {undefined, Req2} ->
			{ok, Req3} = cowboy_req:reply(404, Req2),
			{State, Req3};
		    {ReqJson, Req2} ->
			HandleJsonCall(Req2, ReqJson)
		end;
	    {_, Req1} -> 
		{ok, Req2} = cowboy_req:reply(405, Req1),
		{State, Req2}
	end,
    {ok, ReqZ, State1}.

terminate(Reason, _Req, _State) ->
    ?LOG_DEBUG([http_terminate, 
	       {reason, Reason},
	       {state, _State}]),
     ok.

%%------------------------------------------------------------------------
%% REST paragraph
%%------------------------------------------------------------------------
rest_init(Req, Opts) ->
    try 
	{Service, Req1} = resolve_service(Req, Opts),
	TyCache = dict:new(),
	{ok,
	 Req1,
	 #state{service     = Service,
		types_cache = TyCache }}
    catch
	_ : Reason ->
	    ?LOG_ERROR([ resolve_service_failed, 
			 {reason, Reason}, 
			 {stacktrace, erlang:get_stacktrace()}]),
	    cowboy_req:reply(503, Req),
	    {shutdown, Req, undefined}
    end.

%%------------------------------------------------------------------------
%% Websocket paragraph
%%------------------------------------------------------------------------

resolve_service(Req, Opts) ->
    {ServicePath, Req1} = cowboy_req:path_info(Req),
    RMod    = proplists:get_value(resolver, Opts, wsrpc_gs_resolver),
    Service = RMod:resolve(ServicePath),
    ?LOG_DEBUG([{service_resolved, ServicePath}, {options, Opts}]),
    {Service, Req1}.

websocket_init(_Any, Req, Opts) ->
    Service = resolve_service(Req, Opts),
    case Service of
	{gen_server, Pid} -> erlang:monitor(process, Pid);
	_ -> ok
    end,
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

make_reply(ReplyObj, Req, State) ->
    {Json, State1} = make_reply(ReplyObj, State),
    {reply, {text, Json }, Req, State1 }.

make_reply(ReplyObj, #state{types_cache = TyCache} = State) ->
    {Jsx, TyCache1} = jsx_util:to_jsx(
			ReplyObj, TyCache, 
			fun(Type)-> get_type(State, Type) end),
    {jsx:encode(Jsx), 200, State#state{types_cache = TyCache1} }.

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

 
