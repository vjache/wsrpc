-module(wsrpc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, read_priv_file/1, make_lager_message/1]).

start() ->
    start_recursive(wsrpc).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case wsrpc_sup:start_link() of
	{ok, Pid} ->
	    start_webserver(),
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_recursive(Applications) when is_list(Applications) ->
    lists:foreach(
      fun(A) -> ok = start_recursive(A) end,
      Applications);

start_recursive(Application) when is_atom(Application) ->
    try
        case application:load(Application) of
            ok -> ok;
            {error, {already_loaded,Application}} -> ok
        end,
        case application:get_key(Application, applications) of
            {ok, Dependencies} ->
                [start_recursive(A) || A <- Dependencies],
                case application:start(Application) of
                    ok -> ok;
                    {error, {already_started, Application}} -> ok
                end;
            undefined -> ok
        end
    catch
        Class:Exception ->
            Error = {startup_failed, {Class, Exception}},
            timer:sleep(1000),
            throw(Error)
    end.

read_priv_file(RelFilename) ->
    Filename=get_priv_file(RelFilename),
    case file:read_file(Filename) of
	{ok, Bin}       -> Bin;
	{error, Reason} -> throw(Reason)
    end.

get_priv_file(RelFilename) ->
	case code:priv_dir(wsrpc) of
        {error, Reason} ->
            throw(Reason);
        PD -> filename:join(PD, RelFilename)
    end.

start_webserver() ->
    Dispatch = cowboy_router:compile(
		 [ {'_', [
			  {"/echo/[...]", wsrpc_handler, [{resolver, wsrpc_gs_resolver}]},
			  {"/", cowboy_static, 
			   [
			    {directory, {priv_dir, wsrpc, []}},
			    {file, "web/main.html"},
			    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			   ]},
			  {"/[...]", 
			   cowboy_static, 
			   [{directory, {priv_dir, wsrpc, []}},
			    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			   ]}
			 ]}
		 ]),
    {ok, _} = cowboy:start_http(
		http, 1, [{port, 8585}],
		[{env, [{dispatch, Dispatch}]}]).


make_lager_message(Report) when is_list(Report) ->
    string:join([ to_str(E) || E <- Report], ", ").

to_str(A) when is_atom(A) ->
    atom_to_list(A);
to_str({K,V}) ->
    lists:flatten(io_lib:format("~p: ~p", [K,V]));
to_str(Any) ->
    lists:flatten(io_lib:format("~p", [Any])).
