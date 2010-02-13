-module(tr).


-export([test/0,factorial1/1,factorial2/1]).

test() ->
    N = 1000,
    {T1, _} = timer:tc(?MODULE, factorial1, [N]),
    {T2, _} = timer:tc(?MODULE, factorial2, [N]),
    io:format("~p~n~p~n", [T1, T2]),
    halt().

% no tail recursion
factorial1(N) when N > 1 ->
  N * factorial1(N - 1);
factorial1(N) when N == 1; N == 0 -> 
    1;
factorial1(_) ->
    error.

% tail recursion 
factorial2(N) ->
    factorial2(N, 1).
factorial2(N, R) when N > 1->
    factorial2(N - 1, N * R);
factorial2(N, R) when N == 1; N == 0 -> 
    R;
factorial2(_, _) -> 
    error.
