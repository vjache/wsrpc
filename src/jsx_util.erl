-module(jsx_util).

-include_lib("eunit/include/eunit.hrl").

-export([get_value/2, 
	 get_value/3, 
	 get_values/2,
	 to_jsx/3,
	 to_jsx/2,
	 from_jsx/3,
	 from_jsx/2]).

get_value(Key, JsxObj) ->
    get_value(Key, JsxObj, undefined).

get_value({Key, ConvType, Def}, JsxObj, undefined) ->
    get_value({Key, ConvType}, JsxObj, Def);
get_value({Key, ConvType}, JsxObj, Def) ->
    V = get_value(Key, JsxObj, Def),
    case ConvType of
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
	    to_integer(V)
    end;
get_value(Key, JsxObj, Def) when is_atom(Key) ->
    proplists:get_value(Key, JsxObj,
      proplists:get_value(
	atom_to_binary(Key,latin1), 
	JsxObj, Def));
get_value(Key, JsxObj, Def) when is_binary(Key) ->
    proplists:get_value(atom_to_binary(Key, latin1), JsxObj, Def).

get_values(Keys, JsxObj) when is_list(Keys) ->
    [get_value(Key, JsxObj) || Key <- Keys].

to_atom(A) when is_atom(A) ->
    A;
to_atom(B) when is_binary(B) ->
    binary_to_existing_atom(B, latin1);
to_atom(L) when is_list(L) ->
    list_to_existing_atom(L).

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
	    Type = binary_to_existing_atom(TypeRaw, latin1),
	    case dict:find(Type, Cache) of
		{ok, Fields} -> Cache1 = Cache;
		error ->
		    Fields = GetFields(Type),
		    Cache1 = dict:store(Type, Fields, Cache)
	    end,
	    Values = [ jsx_util:get_value(F, Jsx) || F <- Fields],
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
    Jsx = lists:zip([ type | [case F of {Name, _Type} -> Name; _ -> F end || F <- Fields] ], 
	      [element(1, to_jsx(V, Cache, GetFields) ) 
	       || V <- tuple_to_list(Tuple)] ),
    { Jsx, Cache1};
	    
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
