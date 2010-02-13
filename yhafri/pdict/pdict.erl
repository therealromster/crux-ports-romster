-module(pdict).

-export([init/0]).

-export([get/0, get/1,
	 put/2,
	 erase/0, erase/1,
	 get_keys/1,
	 info/1]).

init() ->
    spawn(fun() ->
		  register(pdict, self()),
		  loop(dict:new())
	  end).

call(Req) ->
    Ref = make_ref(),
    pdict ! {self(), Ref, Req},
    receive
	{Ref, Reply} ->
	    Reply
    end.

get()       -> call(get).
get(K)      -> call({get, K}).
put(K,V)    -> call({put,K,V}).
erase()     -> call(erase).
erase(K)    -> call({erase,K}).
get_keys(V) -> call({get_keys,V}).
info(Pid)   -> call({info,Pid}).


loop(Dicts) ->
    receive
	{From,Ref,Req} ->
	    {Reply, Dicts1} = handle_req(Req,From,Dicts),
	    From ! {Ref, Reply},
	    loop(Dicts1);
	{'DOWN',_,_,Pid,_} ->
	    loop(dict:erase(Pid, Dicts))
    end.

handle_req(get, Pid, Dicts) ->
    {dict_of(Pid, Dicts), Dicts};
handle_req({get,K}, Pid, Dicts) ->
    {proplists:get_value(K, dict_of(Pid, Dicts), undefined), Dicts};
handle_req({put,Key,Val}, Pid, Dicts) ->
    case dict:find(Pid, Dicts) of
	{ok, KVL} ->
	    {Bef,KVL1} = case [V || {K,V} <- KVL, K =:= Key] of
			     [] ->
				 {undefined, [{Key,Val} | KVL]};
			     [OldV] ->
				 {OldV, [{Key,Val}|KVL -- [{Key,OldV}]]}
			 end,
	    Dicts1 = dict:store(Pid, KVL1, Dicts),
	    {Bef, Dicts1};
	error ->
	    erlang:monitor(process, Pid),
	    {undefined, dict:store(Pid, [{Key, Val}], Dicts)}
    end;
handle_req(erase, Pid, Dicts) ->
    {dict_of(Pid, Dicts), dict:erase(Pid, Dicts)};
handle_req({erase,Key}, Pid, Dicts) ->
    case dict:find(Pid, Dicts) of
	{ok, KVL} ->
	    case [V || {K,V} <- KVL, K =:= Key] of
		[] ->
		    {undefined, Dicts};
		[V] ->
		    {V, dict:store(Pid, KVL -- [{Key,V}], Dicts)}
	    end;
	error ->
	    {undefined, Dicts}
    end;
handle_req({get_keys, Val}, Pid, Dicts) ->
    {[K || {K,V} <- dict_of(Pid, Dicts), V =:= Val], Dicts};
handle_req({info, Pid}, _, Dicts) ->
    {dict_of(Pid, Dicts), Dicts}.

    
dict_of(Pid, Dicts) ->
    case dict:find(Pid, Dicts) of
	{ok, KVL} -> KVL;
	error     -> []
    end.
