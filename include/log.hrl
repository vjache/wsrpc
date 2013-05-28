%% Logging macro. Source line information is included for your convenience.
%% Report :: {Tag::term(), Data::term()}
-define(SELF_APP, fun()->case application:get_application() of {ok,_App} -> _App; undefined -> undefined end end()).

-define(LOG(Level, Report), 
	lager:log(Level, 
		  Report ++ [{module, ?MODULE}, 
			     {line, ?LINE}, 
			     {sid,get(sid)}, 
			     {pid, self()}],
		  wsrpc_app:make_lager_message(Report))).
-define(LOG_INFO(Report), ?LOG(info, Report)).
-define(LOG_ERROR(Report),?LOG(error, Report)).
-define(LOG_WARN(Report), ?LOG(warning, Report)).
-define(LOG_DEBUG(Report),?LOG(debug, Report)).

-define(LOG_DUMP(Message), 
	lager:log(debug, 
		  [{module, ?MODULE}, 
		   {line, ?LINE}, 
		   {sid,get(sid)}, 
		   {pid, self()}],
		  Message)).

-define(echo(Msg), ?LOG(debug, [Msg])).

-compile([{parse_transform, lager_transform}]).
