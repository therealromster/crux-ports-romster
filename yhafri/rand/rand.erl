%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
-module(rand).
-author("Paulo Sergio Almeida <psa@di.uminho.pt>").

-export([start/0, start/1, stop/0]).
-export([uniform/0, uniform/1, uniform_vector/1]).
-export([permutation/1, weighted_permutation/1]).

-compile({inline, [{uniform_s,1}, {uniform_s,2}]}).

%%% Utilities for randomness.

%%% Random number generator server.
%%% Used to obtain a single random number sequence in an application.
%%% Provides uniform/0 and uniform/1 as the random module.
%%% Also provides functions to generate a random vector in a single
%%% invocation (minimizing msg passing), and to generate random
%%% permutations and weighted random permutations of lists.
%%%
%%% Uses same method as random module from stdlib:
%%% The method is attributed to B.A. Wichmann and I.D.Hill
%%% See "An efficient and portable pseudo-random number generator",
%%% Journal of Applied Statistics. AS183. 1982. Also Byte March 1987.


%%% interface functions

start() ->
  Seed = {3172, 9814, 20125},  % random:seed0
  start(Seed).

start(Seed) ->
  Pid = spawn(fun() -> loop(Seed) end),
  case catch register(rand, Pid) of
    true -> ok;
    _ -> exit(Pid, kill), ok
  end.

stop() ->
  rand ! {stop, self()}.

uniform() ->
  rand ! {uniform, self()},
  receive
    {rand, Val} -> Val
  end.

uniform(N) ->
  rand ! {uniform, N, self()},
  receive
    {rand, Val} -> Val
  end.

uniform_vector(N) ->
  rand ! {uniform_vector, N, self()},
  receive
    {rand, Val} -> Val
  end.

%% permutation(List) ->
%% Creates a random permutation of the list;
permutation(List) ->
  Rands = uniform_vector(length(List)),
  Perm = lists:sort(rzip(Rands, List, [])),
  [X || {_W, X} <- Perm].
 
rzip([X | T1], [Y | T2], Acc) ->
  rzip(T1, T2, [{X, Y} | Acc]);
rzip([], [], Acc)  -> Acc.

%% weighted_permutation(List) ->
%% Receives a list of {Elem, Weight} and
%% creates a random permutation of the list according to weights;
%%
%% Based on "Weighted random sampling with a reservoir"
%% By P. S. Efraimidis and P. G. Spirakis
%% Information Processing Letters Volume 97, Issue 5 (March 2006)
%%
%% Elements with more weight appear with higher probability first in the
%% list.
weighted_permutation(List) ->
  Rands = uniform_vector(length(List)),
  Perm = lists:sort(weigh(List, Rands, [])),
  [X || {_W, X} <- Perm].

weigh([{X, W} | T1], [R | T2], Acc) when W /= 0 ->
  weigh(T1, T2, [{-math:pow(R, 1/W), X} | Acc]);
weigh([{X, _W} | T1], [_R | T2], Acc) ->
  weigh(T1, T2, [{0.0, X} | Acc]);
weigh([], [], Acc)  -> Acc.


%%% server loop

loop(Seed) ->
  {Val, NewSeed} =
    receive
      {uniform, From} -> uniform_s(Seed);
      {uniform, N, From} -> uniform_s(N, Seed);
      {uniform_vector, N, From} -> univect(N, Seed, []);
      {stop, From} -> exit(normal)
    end,
  From ! {rand, Val},
  loop(NewSeed).


%%% implementation

univect(0, Seed, Acc) -> {Acc, Seed};
univect(N, Seed, Acc) ->
  {Val, NewSeed} = uniform_s(Seed),
  univect(N-1, NewSeed, [Val | Acc]).

%% same as random module in stdlib; copied here to avoid inter-module calls
%% and be able to inline

uniform_s({A1, A2, A3}) ->
    B1 = (A1*171) rem 30269,
    B2 = (A2*172) rem 30307,
    B3 = (A3*170) rem 30323,
    R = A1/30269 + A2/30307 + A3/30323,
    {R - trunc(R), {B1,B2,B3}}.

uniform_s(N, State0) when N >= 1 ->
    {F, State1} = uniform_s(State0),
    {trunc(F * N) + 1, State1}.


