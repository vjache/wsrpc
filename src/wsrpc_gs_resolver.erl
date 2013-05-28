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

handle_call(JsxMesg, {Pid, _} = _From, State) ->
    case proplists:get_value(<<"method">>,JsxMesg) of
	<<"subscribe">> -> 
	    SPid = spawn(fun()->
				 [begin 
				      Pid ! {jsx_stream, 
					     [{tick, N}], 
					     self() },
				      timer:sleep(1000)
				  end|| N <- lists:seq(1,10)]
			 end),
	    {reply, {jsx_stream, null, SPid}, State};
	<<"error">> ->
	    {reply, {error, test_error}, State};
	_ ->
	    {reply, {jsx, JsxMesg}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
