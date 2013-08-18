%%%-------------------------------------------------------------------
%%% @author vjache <vjache@zen>
%%% @copyright (C) 2013, vjache
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2013 by vjache <vjache@zen>
%%%-------------------------------------------------------------------
-module(wsrpc_gs_resolver).

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/0, resolve/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).
-record(echo, {text, dt}).
-record(timer, {period, label}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% wsrpc resolver callback function.
%%%===================================================================
resolve(_ServicePath) ->
    {ok, Pid} = start_link(),
    {gen_server, Pid}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({get_type, Type}, _From, State) ->
    {reply, case Type of
		echo  -> [text, {dt, iso8601_datetime}];
		timer -> record_info(fields, timer)
	    end, 
     State};
handle_call(#echo{text = Text} = Msg, _From, State) ->
    { {Y,M,D}, {H,Mn,S} } = calendar:universal_time(),
    {reply, Msg#echo{text = <<Text/binary,Text/binary>>,
		    dt = {datetime, Y,M,D,H,Mn,S,0}}, State};
handle_call(#timer{period = Per} = Msg, From, State) ->
    ?LOG_DEBUG([call_timer, Msg]),
    Res = timer:send_interval(
      Per, {Msg, From}),
    ?LOG_DEBUG([{timer_started, Res}]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info({#timer{label = Lab}, From}, State) ->
    wsrpc_handler:stream(From, Lab),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG_DEBUG([service_terminated, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
