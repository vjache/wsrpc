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

-define(s(V), <<V>>).
-define(p(K,V), {K,V}).
-define(sp(K,V), ?p(K, ?s(V))).

-record(state, {service :: {gen_server, pid()} | 
			   {mfa, 
			    Mod :: atom(), 
			    Func :: atom(), 
			    MandatoryArgs :: [] },
		streams = []}).

-record(stream_info, {pid, rid}).

-record(rpc_request, 
	{type :: 'call' | 'call-stream' | 'notify', 
	 rid  :: integer(), 
	 data :: any()}).

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
    %% {ok, Req2} = cowboy_req:reply(
    %% 		   200, [{<<"content-type">>, <<"text/html">>}],
    %% 		   xenob_app:read_
%%		   priv_file("web-ui/main.html"), Req),
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
    RMod    = proplists:get_value(resolver, Opts, wsrpc_simple_resolver),
    Service = RMod:resolve(ServicePath),
    ?LOG_DEBUG([{ws_init, ServicePath, Opts}]),
    {ok, 
     cowboy_req:compact(Req), 
     #state{service = Service}, hibernate}.

websocket_handle({text, JsonCmd}, Req, State) ->
    ?LOG_DEBUG([{request, JsonCmd}]),
    Reply=fun(JSONData, State1) ->
		  ?LOG_DEBUG([reply, {jsx_data, JSONData}]),
		  {reply, {text, to_json(JSONData)}, Req, State1} 
	  end,
    case handle_command(parse_json_command(JsonCmd), State) of
	{noreply,   State1} -> {noreply, Req, State1};
	{JSONReply, State1} ->  Reply(JSONReply, State1)
    end;
websocket_handle(_Any, Req, State) ->
    ?LOG_ERROR([{unexpected_cmd, _Any}]),
    {ok, Req, State}.

websocket_info({'DOWN', _MRef, process, Pid, Reason}, 
	       Req, #state{streams = Streams} = State) ->
    case lists:keytake(Pid, #stream_info.pid, Streams) of 
	{value, #stream_info{rid = Rid}, Streams1} ->
	    ?LOG_ERROR([{streamer_terminated, Pid}, 
			{rid, Rid}, 
			{reason,Reason}]),
	    {reply, 
	     {text, 
	      to_json([?sp(type, "stream-end"), 
		       ?p(rid, Rid) ]) }, Req, State#state{streams = Streams1}};
	false ->
	    {noreply, Req, State}
    end;
websocket_info({jsx_stream, _, _} = Info, Req, State) ->
    Reply=fun(JsxData) ->		  
		  ?LOG_DEBUG([notify, {jsx_data, JsxData}]),
		  {reply, {text, to_json(JsxData)}, Req, State}
	  end,
    JsxData = handle_stream_data(Info, State),
    Reply(JsxData).

websocket_terminate(TermMsg, _Req, _State) ->
    case TermMsg of
	{normal, closed} -> ok;
	_Reason -> 
	    ?LOG_ERROR([{ws_terminate, _Req}, {reason, _Reason}]),
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_stream_data(
  {jsx_stream, Data, StreamPid} = Msg, 
  #state{streams = Streams}) ->
    case lists:keyfind(StreamPid, #stream_info.pid, Streams) of
	#stream_info{rid = Rid} ->
	    [?sp(type, "stream-item"), 
	     ?p(rid,Rid), 
	     ?p(data,Data)];
	false ->
	    ?LOG_ERROR([{bad_stream, StreamPid}, {data, Msg}]),
	    exit({bad_stream, StreamPid})
    end.

to_pretty_binary(Term) ->
    list_to_binary(
      io_lib:format("~p", [Term] )).

handle_command(#rpc_request{type = 'notify',  
			    data = Data} = Req,
	       #state{service = Service} = State) ->
    ?LOG_DEBUG([{rpc_request, Req}]),
    ok = cast_service(Service, Data),
    {noreply, State};
handle_command(#rpc_request{type = 'call', 
			    rid  = Rid, 
			    data = Data} = Req,
	       #state{service = Service, streams = Streams} = State) ->
    ?LOG_DEBUG([{rpc_request, Req}]),
    case call_service(Service, Data) of
	{jsx, ReplyJsxData} ->
	     {[?sp(type, "result"),
	       ?p(rid, Rid),
	       ?p(data, ReplyJsxData)],
	      State};
	{jsx_stream, ReplyJsxData, StreamPid} ->
	    % Ensure there is no two streamers with the same Pid
	    false = lists:keyfind(StreamPid, #stream_info.pid, Streams),
	    erlang:monitor(process, StreamPid),
	    Streams1 = [#stream_info{pid=StreamPid,rid=Rid} | Streams],
	    {[?sp(type, "stream-start"),
	      ?p(rid, Rid),
	      ?p(data, ReplyJsxData)],
	     State#state{streams = Streams1}};
	{error, JsxReason} ->
	    {[?sp(type, "error"),
	       ?p(rid, Rid),
	       ?p(data, JsxReason )], 
	     State}
    end.

call_service({gen_server, Pid}, Data) ->
    gen_server:call(Pid, Data);
call_service({mfa, Mod, Func, Args}, Data) ->
    erlang:apply(Mod, Func, Args ++ [Data]).

cast_service({gen_server, Pid}, Data) ->
    ok = gen_server:cast(Pid, Data);
cast_service({mfa, _Mod, _Func, _Args}, _Data) ->
    {error, cast_not_supported}.


parse_json_command(Json) ->
    JsonData = jsx:decode(Json),
    Type     = binary_to_existing_atom(
		 proplists:get_value(<<"type">>, JsonData), latin1),
    Rid      = proplists:get_value(<<"rid">>, JsonData),
    true     = is_integer(Rid),
    Data     = proplists:get_value(<<"data">>, JsonData),
    #rpc_request{type = Type,
		 rid  = Rid,
		 data = Data}.

to_json(JsonData) ->
    jsx:encode(JsonData).
 
