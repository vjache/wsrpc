-module(wsrpc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, read_priv_file/1, make_lager_message/1]).

-include("log.hrl").

-define(APPLICATION, wsrpc).

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
		 [{'_', 
		   [{"/wsrpc.js", cowboy_static, 
		     [
		      {directory, {priv_dir, wsrpc, []}},
		      {file, "web/js/wsrpc.js"},
		      {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
		     ]} 
		    | make_dispatch()]
		  }]),
    ?LOG_DEBUG([{dispatch_rules, Dispatch}]),
    HttpPort = get_env({http_port, integer}, 8585),
    ?LOG_INFO([{start_http_server, cowboy}, {port, HttpPort}]),
    {ok, _} = cowboy:start_http(
		http, 1, [{port, HttpPort}],
		[{env, [{dispatch, Dispatch}]},
		 {onrequest, fun(Req) -> ?LOG_DEBUG([request, Req]), Req end}]).

make_dispatch() ->
    Apps = get_env({apps, list}, [wsrpc]),
    MakeAppDispatch = 
	fun(AppName, Services) ->
		CommInfo = [dispatch_rule, {app, AppName}],
		AppNameStr = atom_to_list(AppName),
		case application:load(AppName) of
		    ok -> ok;
		    {error, {already_loaded,_}} -> ok
		end,
		MakeServiceDispatch = 
		    fun(ServiceDispatchRule, Resolver) ->
			    ?LOG_INFO(CommInfo ++
					  [{service_resolver, Resolver},
					   {service_web_path, ServiceDispatchRule}]),
			    {ServiceDispatchRule, 
			     wsrpc_handler, 
			     [{resolver, Resolver}]}
		    end,
		AppWebPath = "/" ++ AppNameStr,
		
		[ case Service of
		      {Resolver, ServiceDispatchRule} ->
			  MakeServiceDispatch(ServiceDispatchRule, Resolver);
		      Resolver when is_atom(Resolver) ->
			  MakeServiceDispatch(
			    AppWebPath ++ "/" ++ atom_to_list(Resolver) ++ "/[...]", 
			    Resolver)
		  end || Service <- Services] ++ 
		    [begin
			 AppStaticsWebPath = AppWebPath ++ "/[...]",
			 ?LOG_INFO(CommInfo ++
				       [{static_resources, 
					 {dir, filename:join([AppName, priv, web])},
					 {web_path, AppStaticsWebPath}
					}]),
			 {AppStaticsWebPath, cowboy_static, 
			  [
			   {directory, {priv_dir, AppName, [<<"web">>]}},
			   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			  ]}
		     end]
	end,	      
    lists:flatten(
      [ case App of
	    _ when is_atom(App) ->
		MakeAppDispatch(App, []);
	    {AppName, Opts} when is_atom(AppName), is_list(Opts) ->
		MakeAppDispatch(AppName, 
				proplists:get_value(services, Opts, []))
	end || App <- Apps]).
    

%% get_env(VarName) ->
%%     get_env(VarName,fun()-> throw({var_not_configured,VarName}) end).

get_env({VarName, TypeToCheck} = Spec, Fallback) ->
    Val = get_env(VarName, Fallback),
    Succ = case TypeToCheck of
	integer -> is_integer(Val);
	atom    -> is_atom(Val);
	list    -> is_list(Val);
	binary  -> is_binary(Val);
	tuple   -> is_tuple(Val)
    end,
    not Succ andalso exit({badtype, Spec, Val}),
    Val;
get_env(VarName, Fallback) ->
    case application:get_env(?APPLICATION,VarName) of
        {ok, Value} -> Value;
        undefined -> 
            if is_function(Fallback) -> Fallback();
               true -> Fallback
            end
    end.


make_lager_message(Report) when is_list(Report) ->
    string:join([ to_str(E) || E <- Report], ", ").

to_str(A) when is_atom(A) ->
    atom_to_list(A);
to_str({K,V}) ->
    lists:flatten(io_lib:format("~p: ~p", [K,V]));
to_str(Any) ->
    lists:flatten(io_lib:format("~p", [Any])).
