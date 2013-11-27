-module(jsx_util).

-include_lib("eunit/include/eunit.hrl").

-export([get_value/2, 
	 get_value/3, 
	 get_values/2,
	 to_jsx/3,
	 to_jsx/2,
	 from_jsx/3,
	 from_jsx/2]).

-export([ format_iso8601_datetime/1, parse_iso8601_datetime/1 ]).

-record(datetime,
        {year  = 0 :: non_neg_integer(),
         month = 0 :: non_neg_integer(),
         day   = 0 :: non_neg_integer(),
         hour  = 0 :: non_neg_integer(),
         min   = 0 :: non_neg_integer(),
         sec   = 0 :: non_neg_integer(),
         mls   = 0 :: non_neg_integer(),
         tz    = 0 :: non_neg_integer()}).

get_value(Key, JsxObj) ->
    get_value(Key, JsxObj, undefined).

get_value({Key, ConvType, Def}, JsxObj, undefined) ->
    get_value({Key, ConvType}, JsxObj, Def);
get_value({Key, ConvType}, JsxObj, Def) ->
    case get_value(Key, JsxObj, Def) of
	[{_,_}|_] = V -> V;
	V ->
	    if V == undefined; V == <<"null">> ->
		    undefined;
	       true ->
		    case ConvType of
			jsx_data ->
			    V;
			atom ->
			    to_atom(V);
			atoms ->
			    [to_atom(X) || X <- V];
			list ->
			    to_list(V);
			lists ->
			    [to_list(X) || X <- V];
			binary ->
			    to_binary(V);
			integer ->
			    to_integer(V);
			iso8601_datetime ->
			    parse_iso8601_datetime(V);
			iso8601_duration ->
			    parse_iso8601_duration(V)
		    end
	    end
    end;
get_value(Key, JsxObj, Def) when is_atom(Key) ->
    proplists:get_value(Key, JsxObj,
      proplists:get_value(
	atom_to_binary(Key,latin1), 
	JsxObj, Def));
get_value(Key, JsxObj, Def) when is_binary(Key) ->
    proplists:get_value(Key, JsxObj,
      proplists:get_value(
	binary_to_atom(Key, latin1), 
	JsxObj, Def)).

get_values(Keys, JsxObj) when is_list(Keys) ->
    [get_value(Key, JsxObj) || Key <- Keys].

to_atom(A) when is_atom(A) ->
    A;
to_atom(B) when is_binary(B) ->
    binary_to_atom(B, latin1);
to_atom(L) when is_list(L) ->
    list_to_atom(L).

to_list(L) when is_list(L) ->
    L;
to_list(B) when is_binary(B)->
    binary_to_list(B);
to_list(A) when is_atom(A) ->
    atom_to_list(A);
to_list(N) when is_number(N) ->
    lists:flatten(io_lib:format("~p", [N])).

to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1).

to_integer(I) when is_integer(I) ->
    I;
to_integer(L) when is_list(L) ->
    list_to_integer(L);
to_integer(B) when is_binary(B) ->
    to_integer(binary_to_list(B)).


from_jsx(Jsx, GetFields) when is_function(GetFields, 1) ->
    {Obj, _} = from_jsx(Jsx, dict:new(), GetFields),
    Obj;
from_jsx(Jsx, Cache) when element(1, Cache) == dict ->
    {Obj, _} = from_jsx(Jsx, Cache, 
		      fun(Type) -> exit({no_type_info, Type}) end),
    Obj.

from_jsx([{_,_}|_]=Jsx, Cache, GetFields) when is_function(GetFields, 1)  ->
    case jsx_util:get_value(type, Jsx) of
	undefined ->
	    {Jsx, Cache};
	TypeRaw ->
	    Type = try binary_to_existing_atom(TypeRaw, latin1)
		   catch _:badarg -> exit({unknown_type, TypeRaw}) end,
	    case dict:find(Type, Cache) of
		{ok, Fields} -> Cache1 = Cache;
		error ->
		    Fields = GetFields(Type),
		    Cache1 = dict:store(Type, Fields, Cache)
	    end,
	    Values = [ get_value(F, Jsx) || F <- Fields],
	    {Values1, Cache2} = 
		lists:mapfoldl(
		  fun(V, C) -> from_jsx(V, C, GetFields) end, 
		  Cache1, Values),
	    {list_to_tuple([Type | Values1 ]), Cache2}
    end;
from_jsx([{}], Cache, _) ->
    {null, Cache};
from_jsx([_|_] = L, Cache, GetFields) ->
    lists:mapfoldl(fun(E,A)-> from_jsx(E,A,GetFields) end, Cache, L);
from_jsx(Jsx, Cache, _) ->
    {Jsx, Cache}.

    
to_jsx(Tuple, GetFields) when is_function(GetFields, 1) ->
    {Jsx, _} = to_jsx(Tuple, dict:new(), GetFields),
    Jsx;
to_jsx(Tuple, Cache) when element(1, Cache) == dict ->
    {Jsx, _} = to_jsx(Tuple, Cache, 
		      fun(Type) -> exit({no_type_info, Type}) end),
    Jsx.

to_jsx(#datetime{} = DT, _C, _) ->
    {format_iso8601_datetime(DT), _C};
to_jsx({duration, _, _, _, _, _, _ } = DR, _C, _) ->
    {format_iso8601_duration(DR), _C};
to_jsx({jsx_data, Jsx }, _C, _) ->
    {Jsx, _C};

to_jsx(Tuple, Cache, GetFields) when is_atom(element(1,Tuple)), 
				     is_function(GetFields, 1) ->
    Type = element(1,Tuple),
    case dict:find(Type, Cache) of
	{ok, Fields} ->
	    Cache1 = Cache;
	error   ->
	    Fields = GetFields(Type),
	    (length(Fields) == size(Tuple) - 1) 
		orelse exit(arity_mismatch),
	    Cache1 = dict:store(Type, Fields, Cache)
    end,
    {Jsx, Cache2} = lists:mapfoldl(
		      fun({type, Val}, C) ->
			   { {type, to_binary(Val)}, C};
			 ({{FieldName, FieldType} = Finfo, Val}, C) 
			    when is_atom(FieldName) ->
			      if is_atom(element(1,Val)) ->
				      {Val1, C1} = to_jsx(Val, C, GetFields),
				      { {FieldName, Val1}, C1};
				 true ->
				      try case FieldType of
					      list     -> { {FieldName, to_binary(Val)}, C};
					      lists    -> { {FieldName, [to_binary(V)||V<-Val]}, C};
					      binary   -> { {FieldName, to_binary(Val)}, C};
					      atom     -> { {FieldName, to_binary(Val)}, C};
					      atoms    -> { {FieldName, [to_binary(V)||V<-Val]}, C};
					      jsx_data -> { {FieldName, Val}, C};
					      _ -> {Val1, C1} = to_jsx(Val, C, GetFields),
						   { {FieldName, Val1}, C1}
					  end
				      catch _:Reason ->
					      exit({prop_failed_to_jsx, 
						    {Type, Finfo}, 
						    erlang:get_stacktrace()}) 
				      end
			      end;
			 ({FieldName, Val}, C) when is_atom(FieldName) ->
			      {Val1, C1} = to_jsx(Val, C, GetFields),
			      { {FieldName, Val1}, C1}
		      end, Cache1, 
		      lists:zip([ type | Fields ], tuple_to_list(Tuple) ) ),
    { Jsx, Cache2};

to_jsx({X,Y}, _C,_) when is_integer(X),
		   is_integer(Y) ->
    {[{x, X}, 
      {y, Y}], _C};
to_jsx(B, _C, _) when is_binary(B) ->
    {B, _C};
to_jsx(A, _C, _) when is_atom(A) ->
    {atom_to_binary(A, latin1), _C};
to_jsx(L, _C, _F) when is_list(L) ->
    lists:mapfoldl(fun(E,A)-> to_jsx(E,A,_F) end, _C, L);
to_jsx(N, _C, _) when is_number(N) ->
    {N, _C}.

to_jsx_test() ->
    GetFields = fun(rec1) ->
			[a,b,c];
		   (rec2) ->
			[d,e]
		end,
    to_jsx([{rec1, 1, two, <<"three">>},
	    {rec2, 1, two},
	    {rec1, 2, four, <<"five">>}],dict:new(),GetFields).

from_jsx_test() ->
    GetFields = fun(rec1) ->
			[a,{b,atom},c];
		   (rec2) ->
			[d,{e, list}]
		end,
    from_jsx([[{type,<<"rec1">>},{a,1},{b,<<"two">>},
	       {c,
		[[{type,<<"rec2">>},{d,1},{e,<<"two">>}],
		 [{type,<<"rec2">>},{d,2},{<<"e">>,<<"two">>}]]}],
	      [{type,<<"rec2">>},{d,1},{e,<<"two">>}],
	      [{type,<<"rec1">>},{a,2},{b,<<"four">>},{c,<<"five">>}]],
	     dict:new(),GetFields).

parse_iso8601_datetime(
  <<YYYY:4/binary,"-",
    MM:2/binary,"-",
    DD:2/binary,"T",
    HH:2/binary,":",
    MN:2/binary,":",
    SS:2/binary,".",MLS:3/binary,S,TZ:2/binary>>) ->
    Year = to_integer(YYYY),
    Month= to_integer(MM),
    Day  = to_integer(DD),
    Hour = to_integer(HH),
    Min  = to_integer(MN),
    Sec  = to_integer(SS),
    Mls  = to_integer(MLS),
    TZone= (case S of $- -> -1; $+ -> 1 end) * to_integer(TZ),
    make_datetime(Year, Month, Day, Hour, Min, Sec, Mls, TZone);
parse_iso8601_datetime(
  <<YYYY:4/binary,"-",
    MM:2/binary,"-",
    DD:2/binary,"T",
    HH:2/binary,":",
    MN:2/binary,":",
    SS:2/binary,".", MLS:3/binary,"Z">>) ->
    Year = to_integer(YYYY),
    Month= to_integer(MM),
    Day  = to_integer(DD),
    Hour = to_integer(HH),
    Min  = to_integer(MN),
    Sec  = to_integer(SS),
    Mls  = to_integer(MLS),
    make_datetime(Year, Month, Day, Hour, Min, Sec, Mls, 0);
parse_iso8601_datetime(
  <<_YYYY:4/binary,"-",
    _MM:2/binary,"-",
    _DD:2/binary,"T",
    _HH:2/binary,":",
    _MN:2/binary,":",
    _SS:2/binary,".", _MLS:3/binary>> = B) ->
    parse_iso8601_datetime(<<B/binary,"Z">>);
parse_iso8601_datetime(
  <<_YYYY:4/binary,"-",
    _MM:2/binary,"-",
    _DD:2/binary,"T",
    _HH:2/binary,":",
    _MN:2/binary,":",
    _SS:2/binary>> = B) ->
    parse_iso8601_datetime(<<B/binary,".000Z">>);
parse_iso8601_datetime(
  <<YYYY:4/binary,"-",
    MM:2/binary,"-",
    DD:2/binary>>) ->
    Year = to_integer(YYYY),
    Month= to_integer(MM),
    Day  = to_integer(DD),
    make_datetime(Year, Month, Day, 0, 0, 0, 0, 0).

make_datetime(Year, Month, Day, Hour, Min, Sec, Mls, TZone) ->
    #datetime{year = Year, month = Month, day = Day, hour = Hour, min = Min, sec = Sec, mls = Mls, tz = TZone }.

parse_iso8601_duration(Dur) when is_binary(Dur) ->
    parse_iso8601_duration(binary_to_list(Dur));
parse_iso8601_duration(Dur) when is_list(Dur) ->
    case re:run(Dur, 
	   "P(\\d*Y)?(\\d*M)?(\\d*D)?T(\\d*H)?(\\d*M)?(\\d*S)?", 
	   [{capture, all_but_first, list}]) of
	{match, L } ->
	    [Y,M,D,H,Mn,S] = 
		[ case V of
		      "" -> 0;
		      _ -> 
			  to_integer(
			    lists:reverse(
			      tl(lists:reverse(V)))) 
		  end || V <- L ],
	    {duration, Y, M, D, H, Mn, S};
	nomatch ->
	    exit({not_an_iso8601_duration, Dur})
    end.

format_iso8601_datetime(
  #datetime{year = Year, month = Month, day = Day, hour = Hour, min = Min, sec = Sec, mls = Mls, tz = TZone }) ->
        iolist_to_binary(
	  io_lib:format(
	    "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.~.3.0w~s~.2.0w",
	    [Year, Month, Day, Hour, Min, Sec, Mls, if TZone >= 0 -> "+"; TZone =< 0 -> "-" end, abs(TZone)] )).

format_iso8601_duration({duration, Y, M, D, H, Mn, S}) ->
    iolist_to_binary(
      io_lib:format("P~wY~wM~wDT~wH~wM~wS", 
		    [Y, M, D, H, Mn, S] ) ).

