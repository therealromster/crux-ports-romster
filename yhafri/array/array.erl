%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id:$
%%
%% @copyright 2006 Richard Carlsson, Dan Gudmundsson
%% @author Richard Carlsson <richardc@it.uu.se>
%% @author Dan Gudmundsson <dgud@erix.ericsson.se>
%% @version 0.9
%% @doc Functional, extendible arrays.

%% @type array(). A functional, extendible array. The representation is
%% not documented and is subject to change without notice. Note that
%% arrays cannot be directly compared for equality.

-module(array).

-export([new/0, new/1, new/2, set/3, get/2, size/1, default/1, reset/2,
	 to_list/1, sparse_to_list/1, from_list/1, from_list/2,
	 to_orddict/1, sparse_to_orddict/1, from_orddict/1,
	 from_orddict/2, map/2, sparse_map/2, foldl/3, foldr/3,
	 sparse_foldl/3, sparse_foldr/3]).

-ifndef(NOTEST).
-include_lib("eunit/include/eunit_test.hrl").
-endif.

%% Remember: the key to speed is to minimize the number of tests, on
%% large input. Always make the most probable path as short as possible.
%% In particular, keep in mind that for large trees, the probability of
%% a leaf node is small relative to that of an internal node.
%%
%% If you try to tweak the set_1 and get_1 loops: Measure, look at the
%% generated Beam code, and measure again! The argument order matters!


%% Representation:
%%
%% A tree is either a leaf, with LEAFSIZE elements (the "base"), an
%% internal node with LEAFSIZE+1 elements, or an unexpanded tree,
%% represented by a single integer: the number of elements that may be
%% stored in the tree when it is expanded. The last element of an
%% internal node caches the number of elements that may be stored in
%% each of its subtrees.
%%
%% Note that to update an entry in a tree of height h = log[b] n, the
%% total number of written words is (b+1)+(h-1)*(b+2), since tuples use
%% a header word on the heap. 4 is the optimal base for minimizing the
%% number of words written, but causes higher trees, which takes time.
%% The best compromise between speed and memory usage seems to lie
%% around 8-10. Measurements indicate that the optimum base for speed is
%% 24 - above that, it gets slower again due to the high memory usage.
%% Base 10 is a good choice, giving 2/3 of the possible speedup from
%% base 4, but only using 1/3 more memory. (Base 24 uses 65% more memory
%% per write than base 10, but the speedup is only 21%.)

-define(DEFAULT, undefined).
-define(LEAFSIZE, 10).		% the "base"
-define(NODESIZE, ?LEAFSIZE).   % (no reason to have a different size)
-define(NODEPATTERN(S), {_,_,_,_,_,_,_,_,_,_,S}). % NODESIZE+1 elements!
-define(NEW_NODE(S),  % beware of argument duplication!
	setelement((?NODESIZE+1),erlang:make_tuple((?NODESIZE+1),(S)),(S))).
-define(NEW_LEAF(D), erlang:make_tuple(?LEAFSIZE,(D))).

%% These make the code a little easier to experiment with.
%% It turns out that using shifts (when LEAFSIZE=2^n) is not faster.
-define(reduce(X), ((X) div (?NODESIZE))).
-define(extend(X), ((X) * (?NODESIZE))).

-record(array, {size,		%% number of defined entries
		max,		%% maximum number of entries in current tree
		default,	%% the default value (usually 'undefined')
		elements	%% the tuple tree
	       }).


%% @spec () -> array()
%% @equiv new(0, undefined)
%%
%% @see new/1
%% @see new/2

new() ->
    new(0, ?DEFAULT).

%% @spec (integer()) -> array()
%% @equiv new(Size, undefined)
%%
%% @see new/0
%% @see new/2

new(Size) ->
    new(Size, ?DEFAULT).

%% @spec (integer(), term()) -> array()
%% @doc Creates a new array. The array will initially have `Size'
%% entries, numbered from 0 to `Size'-1, whose values are initialized to
%% `Default'. If `Size' is not a nonnegative integer, the call fails
%% with reason `badarg'.
%%
%% `Default' is also used as the value for uninitialized entries as the
%% array grows; for example, if the array has initial size zero, and the
%% first entry to be set has index 10, then all the entries 0-9 will be
%% defined to have the default value.
%%
%% @see new/0
%% @see new/1
%% @see set/3
%% @see get/2
%% @see from_list/2

new(0, Default) ->
    M = ?LEAFSIZE,
    #array{size = 0, max = M, default = Default, elements = M};
new(Size, Default) when is_integer(Size), Size > 0 ->
    M = grow_max(Size - 1, ?LEAFSIZE),
    #array{size = Size, max = M, default = Default, elements = M};
new(_, _) ->
    erlang:error(badarg).


%% Enlarging the array upwards to accommodate an index `I'

grow_max(I, M) when I >= M ->
    grow_max(I, ?extend(M));
grow_max(_I, M) ->
    M.

grow(I, E, _M) when is_integer(E) ->
    M1 = grow_max(I, E),
    {M1, M1};
grow(I, E, M) ->
    grow_1(I, E, M).

grow_1(I, E, M) when I >= M ->
    grow(I, setelement(1, ?NEW_NODE(M), E), ?extend(M));
grow_1(_I, E, M) ->
    {E, M}.


%% @spec (array()) -> integer()
%% @doc Returns the number of entries in the array. Entries are numbered
%% from 0 to size-1; hence, this is also the index of the first unused
%% entry.

size(#array{size = N}) -> N;
size(_) -> erlang:error(badarg).


%% @spec (array()) -> term()
%% @doc Returns the value used for uninitialized entries.
%%
%% @see new/2

default(#array{default = D}) -> D;
default(_) -> erlang:error(badarg).


-ifdef(EUNIT).
new_test_() ->
    N0 = ?LEAFSIZE,
    N01 = N0+1,
    N1 = ?NODESIZE*N0,
    N11 = N1+1,
    N2 = ?NODESIZE*N1,
    [?_assertMatch(#array{size=0,max=N0,default=undefined,elements=N0},
		   new(0)),
     ?_assertMatch(#array{size=N0,max=N0,elements=N0}, new(N0)),
     ?_assertMatch(#array{size=N01,max=N1,elements=N1}, new(N01)),
     ?_assertMatch(#array{size=N1,max=N1,elements=N1}, new(N1)),
     ?_assertMatch(#array{size=N11,max=N2,elements=N2}, new(N11)),
     ?_assertMatch(#array{size=N2, max=N2, default=42,elements=N2},
		   new(N2,42)),
     ?_assert(new() == new(0)),
     ?_assert(new(0) == new(0,undefined)),
     ?_assert(0 == array:size(new())),
     ?_assert(undefined == default(new())),
     ?_assert(new(10) == new(10,undefined)),
     ?_assert(17 == array:size(new(17))),
     ?_assert(4711 == default(new(0,4711)))
    ].
-endif.


%% @spec (integer(), term(), array()) -> array()
%% @doc Sets entry `I' of the array to `Value'. If `I' is not a
%% nonnegative integer, the call fails with reason `badarg'.

set(I, Value, #array{size = N, max = M, default = D, elements = E}=A)
  when is_integer(I), I >= 0 ->
    if I < N ->
	    A#array{elements = set_1(I, E, Value, D)};
       I < M ->
	    A#array{size = I+1, elements = set_1(I, E, Value, D)};
       true ->
	    {E1, M1} = grow(I, E, M),
	    A#array{size = I+1, max = M1,
		    elements = set_1(I, E1, Value, D)}
    end;
set(_I, _V, _A) ->
    erlang:error(badarg).

%% See get_1/3 for details about switching and the NODEPATTERN macro.

set_1(I, E=?NODEPATTERN(S), X, D) ->
    I1 = I div S + 1,
    setelement(I1, E, set_1(I rem S, element(I1, E), X, D));
set_1(I, E, X, D) when is_integer(E) ->
    expand(I, E, X, D);
set_1(I, E, X, _D) ->
    setelement(I+1, E, X).


%% Insert an element in an unexpanded node, expanding it as necessary.

expand(I, S, X, D) when S > ?LEAFSIZE ->
    S1 = ?reduce(S),
    setelement(I div S1 + 1, ?NEW_NODE(S1),
	       expand(I rem S1, S1, X, D));
expand(I, _S, X, D) ->
    setelement(I+1, ?NEW_LEAF(D), X).


%% @spec (integer(), array()) -> term()
%% @doc Returns the value of entry `I'. If `I' is outside the current
%% range of the array, the call fails with reason `badarg'.

get(I, #array{size = N, elements = E, default = D})
  when is_integer(I), I >= 0, I < N ->
    get_1(I, E, D);
get(_I, _A) ->
    erlang:error(badarg).

%% The use of NODEPATTERN(S) to select the right clause is just a hack,
%% but it is the only way to get the maximum speed out of this loop
%% (using the Beam compiler in OTP 11).

get_1(I, E=?NODEPATTERN(S), D) ->
    get_1(I rem S, element(I div S + 1, E), D);
get_1(_I, E, D) when is_integer(E) ->
    D;
get_1(I, E, _D) ->
    element(I+1, E).


%% @spec (integer(), array()) -> array()
%% @doc Sets entry `I' to the default value for the array. This is
%% equivalent to `set(I, default(Array), Array)'.
%%
%% @see new/2
%% @see set/3

%% TODO: a reset_range function

reset(I, Array) ->
    set(I, Array#array.default, Array).


-ifdef(EUNIT).
set_get_test_() ->
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    [?_assertException(error, badarg, array:get(0, new())),
     ?_assert(array:get(0, new(1)) == undefined),
     ?_assertException(error, badarg, array:get(1, new(1))),
     ?_assertException(error, badarg, array:get(-1, new(1))),
     ?_assert(array:size(set(0, 17, new())) == 1),
     ?_assert(array:get(0, set(0, 17, new())) == 17),
     ?_assertException(error, badarg, array:get(1, set(0, 17, new()))),
     ?_assert(array:get(0, set(1, 17, new())) == undefined),
     ?_assert(array:get(N1-2, set(N1-1, 17, new())) == undefined),
     ?_assert(array:size(set(N1-1, 17, new())) == N1),
     ?_assert(array:get(N1-1, set(N1-1, 17, new())) == 17),
     ?_assertException(error, badarg, array:get(N1, set(N1-1, 17, new()))),
     ?_assert(array:size(set(0, 42, set(0, 17, new()))) == 1),
     ?_assert(array:get(0, set(0, 42, set(0, 17, new()))) == 42)
    ].
-endif.


%% @spec (array()) -> list()
%% @doc Converts the array to a list.
%%
%% @see from_list/2
%% @see sparse_to_list/1

to_list(#array{size = 0}) ->
    [];
to_list(#array{size = N, elements = E, default = D}) ->
    to_list_1(E, D, N - 1);
to_list(_) ->
    erlang:error(badarg).

%% this part handles the rightmost subtrees

to_list_1(E=?NODEPATTERN(S), D, I) ->
    N = I div S,
    to_list_3(N, D, to_list_1(element(N+1, E), D, I rem S), E);
to_list_1(E, D, I) when is_integer(E) ->
    push(I+1, D, []);
to_list_1(E, _D, I) ->
    push_tuple(I+1, E, []).

%% this part handles full trees only

to_list_2(E=?NODEPATTERN(_S), D, L) ->
    to_list_3(?NODESIZE, D, L, E);
to_list_2(E, D, L) when is_integer(E) ->
    push(E, D, L);
to_list_2(E, _D, L) ->
    push_tuple(?LEAFSIZE, E, L).

to_list_3(0, _D, L, _E) ->
    L;
to_list_3(N, D, L, E) ->
    to_list_3(N-1, D, to_list_2(element(N, E), D, L), E).

push(0, _E, L) ->
    L;
push(N, E, L) ->
    push(N - 1, E, [E | L]).

push_tuple(0, _T, L) ->
    L;
push_tuple(N, T, L) ->
    push_tuple(N - 1, T, [element(N, T) | L]).


-ifdef(EUNIT).
to_list_test_() ->
    N0 = ?LEAFSIZE,
    [?_assert([] == to_list(new())),
     ?_assert([undefined] == to_list(new(1))),
     ?_assert([undefined,undefined] == to_list(new(2))),
     ?_assert(lists:duplicate(N0,0) == to_list(new(N0,0))),
     ?_assert(lists:duplicate(N0+1,1) == to_list(new(N0+1,1))),
     ?_assert(lists:duplicate(N0+2,2) == to_list(new(N0+2,2))),
     ?_assert(lists:duplicate(666,6) == to_list(new(666,6))),
     ?_assert([1,2,3] == to_list(set(2,3,set(1,2,set(0,1,new()))))),
     ?_assert([3,2,1] == to_list(set(0,3,set(1,2,set(2,1,new()))))),
     ?_assert([1|lists:duplicate(N0-2,0)++[1]] ==
	      to_list(set(N0-1,1,set(0,1,new(0,0))))),
     ?_assert([1|lists:duplicate(N0-1,0)++[1]] ==
	      to_list(set(N0,1,set(0,1,new(0,0))))),
     ?_assert([1|lists:duplicate(N0,0)++[1]] ==
	      to_list(set(N0+1,1,set(0,1,new(0,0)))))
    ].
-endif.


%% @spec (array()) -> list()
%% @doc Converts the array to a list, skipping default-valued entries.
%%
%% @see to_list/1

sparse_to_list(#array{size = 0}) ->
    [];
sparse_to_list(#array{size = N, elements = E, default = D}) ->
    sparse_to_list_1(E, D, N - 1);
sparse_to_list(_) ->
    erlang:error(badarg).

%% see to_list/1 for details

sparse_to_list_1(E=?NODEPATTERN(S), D, I) ->
    N = I div S,
    sparse_to_list_3(N, D,
		     sparse_to_list_1(element(N+1, E), D, I rem S),
		     E);
sparse_to_list_1(E, _D, _I) when is_integer(E) ->
    [];
sparse_to_list_1(E, D, I) ->
    sparse_push_tuple(I+1, D, E, []).

sparse_to_list_2(E=?NODEPATTERN(_S), D, L) ->
    sparse_to_list_3(?NODESIZE, D, L, E);
sparse_to_list_2(E, _D, _L) when is_integer(E) ->
    [];
sparse_to_list_2(E, D, L) ->
    sparse_push_tuple(?LEAFSIZE, D, E, L).

sparse_to_list_3(0, _D, L, _E) ->
    L;
sparse_to_list_3(N, D, L, E) ->
    sparse_to_list_3(N-1, D, sparse_to_list_2(element(N, E), D, L), E).

sparse_push_tuple(0, _D, _T, L) ->
    L;
sparse_push_tuple(N, D, T, L) ->
    case element(N, T) of
	D -> sparse_push_tuple(N - 1, D, T, L);
	E -> sparse_push_tuple(N - 1, D, T, [E | L])
    end.


-ifdef(EUNIT).
sparse_to_list_test_() ->
    N0 = ?LEAFSIZE,
    [?_assert([] == sparse_to_list(new())),
     ?_assert([] == sparse_to_list(new(1))),
     ?_assert([] == sparse_to_list(new(1,0))),
     ?_assert([] == sparse_to_list(new(2))),
     ?_assert([] == sparse_to_list(new(2,0))),
     ?_assert([] == sparse_to_list(new(N0,0))),
     ?_assert([] == sparse_to_list(new(N0+1,1))),
     ?_assert([] == sparse_to_list(new(N0+2,2))),
     ?_assert([] == sparse_to_list(new(666,6))),
     ?_assert([1,2,3] == sparse_to_list(set(2,3,set(1,2,set(0,1,new()))))),
     ?_assert([3,2,1] == sparse_to_list(set(0,3,set(1,2,set(2,1,new()))))),
     ?_assert([0,1] == sparse_to_list(set(N0-1,1,set(0,0,new(0))))),
     ?_assert([0,1] == sparse_to_list(set(N0,1,set(0,0,new(0))))),
     ?_assert([0,1] == sparse_to_list(set(N0+1,1,set(0,0,new(0)))))
    ].
-endif.


%% @spec (list()) -> array()
%% @equiv from_list([], undefined)

from_list(List) ->
    from_list(List, undefined).

%% @spec (list(), term()) -> array()
%% @doc Converts a list to an array. `Default' is used as the value for
%% uninitialized entries as the array grows. If `List' is not a proper
%% list, the call fails with reason `badarg'.
%%
%% @see new/2
%% @see to_list/1

from_list([], Default) ->
    new(0, Default);
from_list(List, Default) when is_list(List) ->
    {E, N, M} = from_list_1(?LEAFSIZE, List, Default, 0, [], []),
    #array{size = N, max = M, default = Default, elements = E};
from_list(_, _) ->
    erlang:error(badarg).

%% Note: A cleaner but slower algorithm is to first take the length of
%% the list and compute the max size of the final tree, and then
%% decompose the list. The below algorithm is almost twice as fast,
%% however.

%% Building the leaf nodes (padding the last one as necessary) and
%% counting the total number of elements.
from_list_1(0, Xs, D, N, As, Es) ->
    E = list_to_tuple(lists:reverse(As)),
    case Xs of
	[] ->
	    case Es of
		[] ->
		    {E, N, ?LEAFSIZE};
		_ ->
		    from_list_2_0(N, [E | Es], ?LEAFSIZE)
	    end;
	[_|_] ->
	    from_list_1(?LEAFSIZE, Xs, D, N, [], [E | Es]);
	_ ->
	    erlang:error(badarg)
    end;
from_list_1(I, Xs, D, N, As, Es) ->
    case Xs of
	[X | Xs1] ->
	    from_list_1(I-1, Xs1, D, N+1, [X | As], Es);
	_ ->
	    from_list_1(I-1, Xs, D, N, [D | As], Es)
    end.

%% Building the internal nodes (note that the input is reversed).
from_list_2_0(N, Es, S) ->
    from_list_2(?NODESIZE, pad((N-1) div S + 1, ?NODESIZE, S, Es),
		S, N, [S], []).

from_list_2(0, Xs, S, N, As, Es) ->
    E = list_to_tuple(As),
    case Xs of
	[] ->
	    case Es of
		[] ->
		    {E, N, ?extend(S)};
		_ ->
		    from_list_2_0(N, lists:reverse([E | Es]),
				  ?extend(S))
	    end;
	_ ->
	    from_list_2(?NODESIZE, Xs, S, N, [S], [E | Es])
    end;
from_list_2(I, [X | Xs], S, N, As, Es) ->
    from_list_2(I-1, Xs, S, N, [X | As], Es).


%% left-padding a list Es with elements P to the nearest multiple of K
%% elements from N (adding 0 to K-1 elements).
pad(N, K, P, Es) ->
    push((K - (N rem K)) rem K, P, Es).


-ifdef(EUNIT).
from_list_test_() ->
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    N2 = ?NODESIZE*N1,
    N3 = ?NODESIZE*N2,
    N4 = ?NODESIZE*N3,
    [?_assert(array:size(from_list([])) == 0),
     ?_assert(array:size(from_list([undefined])) == 1),
     ?_assert(array:size(from_list(lists:seq(1,N1))) == N1),
     ?_assert(to_list(from_list(lists:seq(1,N0))) == lists:seq(1,N0)),
     ?_assert(to_list(from_list(lists:seq(1,N0+1))) == lists:seq(1,N0+1)),
     ?_assert(to_list(from_list(lists:seq(1,N0+2))) == lists:seq(1,N0+2)),
     ?_assert(to_list(from_list(lists:seq(1,N2))) == lists:seq(1,N2)),
     ?_assert(to_list(from_list(lists:seq(1,N2+1))) == lists:seq(1,N2+1)),
     ?_assert(to_list(from_list(lists:seq(0,N3))) == lists:seq(0,N3)),
     ?_assert(to_list(from_list(lists:seq(0,N4))) == lists:seq(0,N4))
    ].
-endif.


%% @spec (array()) -> [{Index::integer(), Value::term()}]
%% @doc Converts the array to an ordered list of pairs `{Index, Value}'.
%%
%% @see from_orddict/2
%% @see sparse_to_orddict/1

to_orddict(#array{size = 0}) ->
    [];
to_orddict(#array{size = N, elements = E, default = D}) ->
    I = N - 1,
    to_orddict_1(E, I, D, I);
to_orddict(_) ->
    erlang:error(badarg).

%% see to_list/1 for comparison

to_orddict_1(E=?NODEPATTERN(S), R, D, I) ->
    N = I div S,
    I1 = I rem S,
    to_orddict_3(N, R - I1 - 1, D,
 		 to_orddict_1(element(N+1, E), R, D, I1),
 		 E, S);
to_orddict_1(E, R, D, I) when is_integer(E) ->
    push_pairs(I+1, R, D, []);
to_orddict_1(E, R, _D, I) ->
    push_tuple_pairs(I+1, R, E, []).

to_orddict_2(E=?NODEPATTERN(S), R, D, L) ->
    to_orddict_3(?NODESIZE, R, D, L, E, S);
to_orddict_2(E, R, D, L) when is_integer(E) ->
    push_pairs(E, R, D, L);
to_orddict_2(E, R, _D, L) ->
    push_tuple_pairs(?LEAFSIZE, R, E, L).

to_orddict_3(0, _R, _D, L, _E, _S) ->
    L;
to_orddict_3(N, R, D, L, E, S) ->
    to_orddict_3(N-1, R - S, D,
 		 to_orddict_2(element(N, E), R, D, L),
 		 E, S).

push_pairs(0, _I, _E, L) ->
    L;
push_pairs(N, I, E, L) ->
    push_pairs(N-1, I-1, E, [{I, E} | L]).

push_tuple_pairs(0, _I, _T, L) ->
    L;
push_tuple_pairs(N, I, T, L) ->
    push_tuple_pairs(N-1, I-1, T, [{I, element(N, T)} | L]).


-ifdef(EUNIT).
to_orddict_test_() ->
    N0 = ?LEAFSIZE,
    [?_assert([] == to_orddict(new())),
     ?_assert([{0,undefined}] == to_orddict(new(1))),
     ?_assert([{0,undefined},{1,undefined}] == to_orddict(new(2))),
     ?_assert([{N,0}||N<-lists:seq(0,N0-1)] == to_orddict(new(N0,0))),
     ?_assert([{N,1}||N<-lists:seq(0,N0)] == to_orddict(new(N0+1,1))),
     ?_assert([{N,2}||N<-lists:seq(0,N0+1)] == to_orddict(new(N0+2,2))),
     ?_assert([{N,6}||N<-lists:seq(0,665)] == to_orddict(new(666,6))),
     ?_assert([{0,1},{1,2},{2,3}] ==
	      to_orddict(set(2,3,set(1,2,set(0,1,new()))))),
     ?_assert([{0,3},{1,2},{2,1}] ==
	      to_orddict(set(0,3,set(1,2,set(2,1,new()))))),
     ?_assert([{0,1}|[{N,0}||N<-lists:seq(1,N0-2)]++[{N0-1,1}]]
	      == to_orddict(set(N0-1,1,set(0,1,new(0,0))))),
     ?_assert([{0,1}|[{N,0}||N<-lists:seq(1,N0-1)]++[{N0,1}]]
	      == to_orddict(set(N0,1,set(0,1,new(0,0))))),
     ?_assert([{0,1}|[{N,0}||N<-lists:seq(1,N0)]++[{N0+1,1}]]
	      == to_orddict(set(N0+1,1,set(0,1,new(0,0)))))
    ].
-endif.


%% @spec (array()) -> [{Index::integer(), Value::term()}]
%% @doc Converts the array to an ordered list of pairs `{Index, Value}',
%% skipping default-valued entries.
%%
%% @see to_orddict/1

sparse_to_orddict(#array{size = 0}) ->
    [];
sparse_to_orddict(#array{size = N, elements = E, default = D}) ->
    I = N - 1,
    sparse_to_orddict_1(E, I, D, I);
sparse_to_orddict(_) ->
    erlang:error(badarg).

%% see to_orddict/1 for details

sparse_to_orddict_1(E=?NODEPATTERN(S), R, D, I) ->
    N = I div S,
    I1 = I rem S,
    sparse_to_orddict_3(N, R - I1 - 1, D,
 		 sparse_to_orddict_1(element(N+1, E), R, D, I1),
 		 E, S);
sparse_to_orddict_1(E, _R, _D, _I) when is_integer(E) ->
    [];
sparse_to_orddict_1(E, R, D, I) ->
    sparse_push_tuple_pairs(I+1, R, D, E, []).

sparse_to_orddict_2(E=?NODEPATTERN(S), R, D, L) ->
    sparse_to_orddict_3(?NODESIZE, R, D, L, E, S);
sparse_to_orddict_2(E, _R, _D, _L) when is_integer(E) ->
    [];
sparse_to_orddict_2(E, R, D, L) ->
    sparse_push_tuple_pairs(?LEAFSIZE, R, D, E, L).

sparse_to_orddict_3(0, _R, _D, L, _E, _S) ->
    L;
sparse_to_orddict_3(N, R, D, L, E, S) ->
    sparse_to_orddict_3(N-1, R - S, D,
 		 sparse_to_orddict_2(element(N, E), R, D, L),
 		 E, S).

sparse_push_tuple_pairs(0, _I, _D, _T, L) ->
    L;
sparse_push_tuple_pairs(N, I, D, T, L) ->
    case element(N, T) of
	D -> sparse_push_tuple_pairs(N-1, I-1, D, T, L);
	E -> sparse_push_tuple_pairs(N-1, I-1, D, T, [{I, E} | L])
    end.


-ifdef(EUNIT).
sparse_to_orddict_test_() ->
    N0 = ?LEAFSIZE,
    [?_assert([] == sparse_to_orddict(new())),
     ?_assert([] == sparse_to_orddict(new(1))),
     ?_assert([] == sparse_to_orddict(new(1,0))),
     ?_assert([] == sparse_to_orddict(new(2))),
     ?_assert([] == sparse_to_orddict(new(2,0))),
     ?_assert([] == sparse_to_orddict(new(N0,0))),
     ?_assert([] == sparse_to_orddict(new(N0+1,1))),
     ?_assert([] == sparse_to_orddict(new(N0+2,2))),
     ?_assert([] == sparse_to_orddict(new(666,6))),
     ?_assert([{0,1},{1,2},{2,3}] ==
	      sparse_to_orddict(set(2,3,set(1,2,set(0,1,new()))))),
     ?_assert([{0,3},{1,2},{2,1}] ==
	      sparse_to_orddict(set(0,3,set(1,2,set(2,1,new()))))),
     ?_assert([{0,1},{N0-1,1}]
	       == sparse_to_orddict(set(N0-1,1,set(0,1,new(0,0))))),
     ?_assert([{0,1},{N0,1}]
	      == sparse_to_orddict(set(N0,1,set(0,1,new(0,0))))),
     ?_assert([{0,1},{N0+1,1}]
	      == sparse_to_orddict(set(N0+1,1,set(0,1,new(0,0)))))
    ].
-endif.


%% @spec (list()) -> array()
%% @equiv from_orddict([], undefined)

from_orddict(Orddict) ->
    from_orddict(Orddict, undefined).

%% @spec (list(), term()) -> array()
%% @doc Converts an ordered list of pairs `{Index, Value}' to a
%% corresponding array. `Default' is used as the value for uninitialized
%% entries as the array grows. If `List' is not a proper, ordered list
%% of pairs whose first elements are nonnegative integers, the call
%% fails with reason `badarg'.
%%
%% @see new/2
%% @see to_orddict/1

from_orddict([], Default) ->
    new(0, Default);
from_orddict(List, Default) when is_list(List) ->
    {E, N, M} = from_orddict_1(?LEAFSIZE, List, 0, Default, 0, [], []),
    #array{size = N, max = M, default = Default, elements = E};
from_orddict(_, _) ->
    erlang:error(badarg).

%% Note: see from_list_1() above for details.

%% Building the leaf nodes (inserts default elements for missing list
%% entries and pads the last tuple if necessary) and counting the total
%% number of elements. We then call from_list_2 to finish the job.
from_orddict_1(0, Xs, Ix, D, N, As, Es) ->
    E = list_to_tuple(lists:reverse(As)),
    case Xs of
	[] ->
	    case Es of
		[] ->
		    {E, N, ?LEAFSIZE};
		_ ->
		    from_list_2_0(N, [E | Es], ?LEAFSIZE)
	    end;
	[_|_] ->
	    from_orddict_1(?LEAFSIZE, Xs, Ix, D, N, [], [E | Es]);
	_ ->
	    erlang:error(badarg)
    end;
from_orddict_1(I, Xs, Ix, D, N, As, Es) ->
    case Xs of
	[{Ix, Val} | Xs1] ->
	    from_orddict_1(I-1, Xs1, Ix+1, D, N+1, [Val | As], Es);
	[{Ix1, _} | _] when is_integer(Ix1), Ix1 > Ix ->
	    from_orddict_1(I-1, Xs, Ix+1, D, N+1, [D | As], Es);
	[_ | _] ->
	    erlang:error({badarg, Xs});
	_ ->
	    from_orddict_1(I-1, Xs, Ix+1, D, N, [D | As], Es)
    end.


-ifdef(EUNIT).
from_orddict_test_() ->
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    N2 = ?NODESIZE*N1,
    N3 = ?NODESIZE*N2,
    N4 = ?NODESIZE*N3,
    [?_assert(array:size(from_orddict([])) == 0),
     ?_assert(array:size(from_orddict([{0,undefined}])) == 1),
     ?_assert(array:size(from_orddict([{N0-1,undefined}])) == N0),
     ?_assert(array:size(from_orddict([{N,0}||N<-lists:seq(0,N1-1)]))
	      == N1),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N0-1)],
		   L == to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N0)],
		   L == to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N2-1)],
		   L == to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N2)],
		   L == to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N3-1)],
		   L == to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N4-1)],
		   L == to_orddict(from_orddict(L))))
    ].
-endif.


%% @spec (Function, array()) -> array()
%%    Function = (Index::integer(), Value::term()) -> term()
%% @doc Maps the given function onto each element of the array. The
%% elements are visited in order from the lowest index to the highest.
%% If `Function' is not a function, the call fails with reason `badarg'.
%%
%% @see foldl/3
%% @see foldr/3
%% @see sparse_map/2

map(Function, Array=#array{size = N, elements = E, default = D})
  when is_function(Function) ->
    if N > 0 ->
	    A = Array#array{elements = []}, % kill reference, for GC
	    A#array{elements = map_1(N-1, E, 0, Function, D)};
       true ->
	    Array
    end;
map(_, _) ->
    erlang:error(badarg).

%% It might be simpler to traverse the array right-to-left, as done e.g.
%% in the to_orddict/2 function, but it is better to guarantee
%% left-to-right application over the elements - that is more likely to
%% be a generally useful property.

map_1(N, E=?NODEPATTERN(S), Ix, F, D) ->
    list_to_tuple(lists:reverse([S | map_2(1, E, Ix, F, D, [],
					   N div S + 1, N rem S, S)]));
map_1(N, E, Ix, F, D) when is_integer(E) ->
    map_1(N, unfold(E, D), Ix, F, D);
map_1(N, E, Ix, F, D) ->
    list_to_tuple(lists:reverse(map_3(1, E, Ix, F, D, N+1, []))).

map_2(I, E, Ix, F, D, L, I, R, _S) ->
    map_2_1(I+1, E, [map_1(R, element(I, E), Ix, F, D) | L]);
map_2(I, E, Ix, F, D, L, N, R, S) ->
    map_2(I+1, E, Ix + S, F, D,
	  [map_1(S-1, element(I, E), Ix, F, D) | L],
	  N, R, S).

map_2_1(I, E, L) when I =< ?NODESIZE ->
    map_2_1(I+1, E, [element(I, E) | L]);
map_2_1(_I, _E, L) ->
    L.

map_3(I, E, Ix, F, D, N, L) when I =< N ->
    map_3(I+1, E, Ix+1, F, D, N, [F(Ix, element(I, E)) | L]);
map_3(I, E, Ix, F, D, N, L) when I =< ?LEAFSIZE ->
    map_3(I+1, E, Ix+1, F, D, N, [D | L]);
map_3(_I, _E, _Ix, _F, _D, _N, L) ->
    L.


unfold(S, _D) when S > ?LEAFSIZE ->
    ?NEW_NODE(?reduce(S));
unfold(_S, D) ->
    ?NEW_LEAF(D).


-ifdef(EUNIT).
map_test_() ->
    Id = fun (_,X) -> X end,
    Plus = fun(N) -> fun (_,X) -> X+N end end,
    [?_assertException(error, badarg, map([], new())),
     ?_assertException(error, badarg, map([], new(10))),
     ?_assert(to_list(map(Id, new())) == []),
     ?_assert(to_list(map(Id, new(1))) == [undefined]),
     ?_assert(to_list(map(Id, new(5,0))) == [0,0,0,0,0]),
     ?_assert(to_list(map(Id, from_list([1,2,3,4]))) == [1,2,3,4]),
     ?_assert(to_list(map(Plus(1), from_list([0,1,2,3]))) == [1,2,3,4]),
     ?_assert(to_list(map(Plus(-1), from_list(lists:seq(1,11))))
	      == lists:seq(0,10)),
     ?_assert(to_list(map(Plus(11), from_list(lists:seq(0,99999))))
	      == lists:seq(11,100010))
    ].
-endif.


%% @spec (Function, array()) -> array()
%%    Function = (Index::integer(), Value::term()) -> term()
%% @doc Maps the given function onto each element of the array, skipping
%% default-valued entries. The elements are visited in order from the
%% lowest index to the highest. If `Function' is not a function, the
%% call fails with reason `badarg'.
%%
%% @see map/2

sparse_map(Function, Array=#array{size = N, elements = E, default = D})
  when is_function(Function) ->
    if N > 0 ->
	    A = Array#array{elements = []}, % kill reference, for GC
	    A#array{elements = sparse_map_1(N-1, E, 0, Function, D)};
       true ->
	    Array
    end;
sparse_map(_, _) ->
    erlang:error(badarg).

%% see map/2 for details
%% TODO: we can probably optimize away the use of div/rem here

sparse_map_1(N, E=?NODEPATTERN(S), Ix, F, D) ->
    list_to_tuple(lists:reverse([S | sparse_map_2(1, E, Ix, F, D, [],
						  N div S + 1,
						  N rem S, S)]));
sparse_map_1(_N, E, _Ix, _F, _D) when is_integer(E) ->
    E;
sparse_map_1(_N, E, Ix, F, D) ->
    list_to_tuple(lists:reverse(sparse_map_3(1, E, Ix, F, D, []))).

sparse_map_2(I, E, Ix, F, D, L, I, R, _S) ->
    sparse_map_2_1(I+1, E,
		   [sparse_map_1(R, element(I, E), Ix, F, D) | L]);
sparse_map_2(I, E, Ix, F, D, L, N, R, S) ->
    sparse_map_2(I+1, E, Ix + S, F, D,
	  [sparse_map_1(S-1, element(I, E), Ix, F, D) | L],
	  N, R, S).

sparse_map_2_1(I, E, L) when I =< ?NODESIZE ->
    sparse_map_2_1(I+1, E, [element(I, E) | L]);
sparse_map_2_1(_I, _E, L) ->
    L.

sparse_map_3(I, T, Ix, F, D, L) when I =< ?LEAFSIZE ->
    case element(I, T) of
	D -> sparse_map_3(I+1, T, Ix+1, F, D, [D | L]);
	E -> sparse_map_3(I+1, T, Ix+1, F, D, [F(Ix, E) | L])
    end;
sparse_map_3(_I, _E, _Ix, _F, _D, L) ->
    L.


-ifdef(EUNIT).
sparse_map_test_() ->
    Id = fun (_,X) -> X end,
    Plus = fun(N) -> fun (_,X) -> X+N end end,
    [?_assertException(error, badarg, sparse_map([], new())),
     ?_assertException(error, badarg, sparse_map([], new(10))),
     ?_assert(to_list(sparse_map(Id, new())) == []),
     ?_assert(to_list(sparse_map(Id, new(1))) == [undefined]),
     ?_assert(to_list(sparse_map(Id, new(5,0))) == [0,0,0,0,0]),
     ?_assert(to_list(sparse_map(Id, from_list([1,2,3,4]))) == [1,2,3,4]),
     ?_assert(to_list(sparse_map(Plus(1), from_list([0,1,2,3]))) == [1,2,3,4]),
     ?_assert(to_list(sparse_map(Plus(-1), from_list(lists:seq(1,11))))
	      == lists:seq(0,10)),
     ?_assert(to_list(sparse_map(Plus(11), from_list(lists:seq(0,99999))))
	      == lists:seq(11,100010)),
     ?_assert(to_list(sparse_map(Plus(1), set(1,1,new(0,0))))
	      == [0,2]),
     ?_assert(to_list(sparse_map(Plus(1), set(3,4,set(0,1,new(0,0)))))
	      == [2,0,0,5]),
     ?_assert(to_list(sparse_map(Plus(1), set(9,9,set(1,1,new(0,0)))))
	      == [0,2,0,0,0,0,0,0,0,10])
    ].
-endif.


%% @spec (Function, InitialAcc::term(), array()) -> term()
%%    Function = (Index::integer(), Value::term(), Acc::term()) ->
%%               term()
%% @doc Folds the elements of the array using the given function and
%% initial accumulator value. The elements are visited in order from the
%% lowest index to the highest. If `Function' is not a function, the
%% call fails with reason `badarg'.
%%
%% @see foldr/3
%% @see map/2
%% @see sparse_foldl/3

foldl(Function, A, #array{size = N, elements = E, default = D})
  when is_function(Function) ->
    if N > 0 ->
	    foldl_1(N-1, E, A, 0, Function, D);
       true ->
	    A
    end;
foldl(_, _, _) ->
    erlang:error(badarg).

foldl_1(N, E=?NODEPATTERN(S), A, Ix, F, D) ->
    foldl_2(1, E, A, Ix, F, D, N div S + 1, N rem S, S);
foldl_1(N, E, A, Ix, F, D) when is_integer(E) ->
    foldl_1(N, unfold(E, D), A, Ix, F, D);
foldl_1(N, E, A, Ix, F, _D) ->
    foldl_3(1, E, A, Ix, F, N+1).

foldl_2(I, E, A, Ix, F, D, I, R, _S) ->
    foldl_1(R, element(I, E), A, Ix, F, D);
foldl_2(I, E, A, Ix, F, D, N, R, S) ->
    foldl_2(I+1, E, foldl_1(S-1, element(I, E), A, Ix, F, D),
	    Ix + S, F, D, N, R, S).

foldl_3(I, E, A, Ix, F, N) when I =< N ->
    foldl_3(I+1, E, F(Ix, element(I, E), A), Ix+1, F, N);
foldl_3(_I, _E, A, _Ix, _F, _N) ->
    A.


-ifdef(EUNIT).
foldl_test_() ->
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    Reverse = fun (_,X,L) -> [X|L] end,
    [?_assertException(error, badarg, foldl([], 0, new())),
     ?_assertException(error, badarg, foldl([], 0, new(10))),
     ?_assert(foldl(Count, 0, new()) == 0),
     ?_assert(foldl(Count, 0, new(1)) == 1),
     ?_assert(foldl(Count, 0, new(10)) == 10),
     ?_assert(foldl(Count, 0, from_list([1,2,3,4])) == 4),
     ?_assert(foldl(Count, 10, from_list([0,1,2,3,4,5,6,7,8,9])) == 20),
     ?_assert(foldl(Count, 1000, from_list(lists:seq(0,999))) == 2000),
     ?_assert(foldl(Sum, 0, from_list(lists:seq(0,10))) == 55),
     ?_assert(foldl(Reverse, [], from_list(lists:seq(0,1000)))
	      == lists:reverse(lists:seq(0,1000)))
    ].
-endif.


%% @spec (Function, InitialAcc::term(), array()) -> term()
%%    Function = (Index::integer(), Value::term(), Acc::term()) ->
%%               term()
%% @doc Folds the elements of the array using the given function and
%% initial accumulator value, skipping default-valued entries. The
%% elements are visited in order from the lowest index to the highest.
%% If `Function' is not a function, the call fails with reason `badarg'.
%%
%% @see foldl/3
%% @see sparse_foldr/3

sparse_foldl(Function, A, #array{size = N, elements = E, default = D})
  when is_function(Function) ->
    if N > 0 ->
	    sparse_foldl_1(N-1, E, A, 0, Function, D);
       true ->
	    A
    end;
sparse_foldl(_, _, _) ->
    erlang:error(badarg).

%% see foldl/3 for details
%% TODO: this can be optimized

sparse_foldl_1(N, E=?NODEPATTERN(S), A, Ix, F, D) ->
    sparse_foldl_2(1, E, A, Ix, F, D, N div S + 1, N rem S, S);
sparse_foldl_1(_N, E, A, _Ix, _F, _D) when is_integer(E) ->
    A;
sparse_foldl_1(N, E, A, Ix, F, D) ->
    sparse_foldl_3(1, E, A, Ix, F, D, N+1).

sparse_foldl_2(I, E, A, Ix, F, D, I, R, _S) ->
    sparse_foldl_1(R, element(I, E), A, Ix, F, D);
sparse_foldl_2(I, E, A, Ix, F, D, N, R, S) ->
    sparse_foldl_2(I+1, E, sparse_foldl_1(S-1, element(I, E), A, Ix, F, D),
	    Ix + S, F, D, N, R, S).

sparse_foldl_3(I, T, A, Ix, F, D, N) when I =< N ->
    case element(I, T) of
	D -> sparse_foldl_3(I+1, T, A, Ix+1, F, D, N);
	E -> sparse_foldl_3(I+1, T, F(Ix, E, A), Ix+1, F, D, N)
    end;
sparse_foldl_3(_I, _T, A, _Ix, _F, _D, _N) ->
    A.


-ifdef(EUNIT).
sparse_foldl_test_() ->
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    Reverse = fun (_,X,L) -> [X|L] end,
    [?_assertException(error, badarg, sparse_foldl([], 0, new())),
     ?_assertException(error, badarg, sparse_foldl([], 0, new(10))),
     ?_assert(sparse_foldl(Count, 0, new()) == 0),
     ?_assert(sparse_foldl(Count, 0, new(1)) == 0),
     ?_assert(sparse_foldl(Count, 0, new(10,1)) == 0),
     ?_assert(sparse_foldl(Count, 0, from_list([0,1,2,3,4],0)) == 4),
     ?_assert(sparse_foldl(Count, 0, from_list([0,1,2,3,4,5,6,7,8,9,0],0))
	      == 9),
     ?_assert(sparse_foldl(Count, 0, from_list(lists:seq(0,999),0))
	      == 999),
     ?_assert(sparse_foldl(Sum, 0, from_list(lists:seq(0,10), 5)) == 50),
     ?_assert(sparse_foldl(Reverse, [], from_list(lists:seq(0,1000), 0))
	      == lists:reverse(lists:seq(1,1000)))
    ].
-endif.


%% @spec (Function, InitialAcc::term(), array()) -> term()
%%    Function = (Index::integer(), Value::term(), Acc::term()) ->
%%               term()
%% @doc Folds the elements of the array right-to-left using the given
%% function and initial accumulator value. The elements are visited in
%% order from the highest index to the lowest. If `Function' is not a
%% function, the call fails with reason `badarg'.
%%
%% @see foldl/3
%% @see map/2

foldr(Function, A, #array{size = N, elements = E, default = D})
  when is_function(Function) ->
    if N > 0 ->
	    I = N - 1,
	    foldr_1(I, E, I, A, Function, D);
       true ->
	    A
    end;
foldr(_, _, _) ->
    erlang:error(badarg).

%% this is based on to_orddict/2

foldr_1(I, E=?NODEPATTERN(S), Ix, A, F, D) ->
    foldr_2(I div S + 1, E, Ix, A, F, D, I rem S, S-1);
foldr_1(I, E, Ix, A, F, D) when is_integer(E) ->
    foldr_1(I, unfold(E, D), Ix, A, F, D);
foldr_1(I, E, Ix, A, F, _D) ->
    I1 = I+1,
    foldr_3(I1, E, Ix-I1, A, F).

foldr_2(0, _E, _Ix, A, _F, _D, _R, _R0) ->
    A;
foldr_2(I, E, Ix, A, F, D, R, R0) ->
    foldr_2(I-1, E, Ix - R - 1,
	    foldr_1(R, element(I, E), Ix, A, F, D),
	    F, D, R0, R0).

foldr_3(0, _E, _Ix, A, _F) ->
    A;
foldr_3(I, E, Ix, A, F) ->
    foldr_3(I-1, E, Ix, F(Ix+I, element(I, E), A), F).


-ifdef(EUNIT).
foldr_test_() ->
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    List = fun (_,X,L) -> [X|L] end,
    [?_assertException(error, badarg, foldr([], 0, new())),
     ?_assertException(error, badarg, foldr([], 0, new(10))),
     ?_assert(foldr(Count, 0, new()) == 0),
     ?_assert(foldr(Count, 0, new(1)) == 1),
     ?_assert(foldr(Count, 0, new(10)) == 10),
     ?_assert(foldr(Count, 0, from_list([1,2,3,4])) == 4),
     ?_assert(foldr(Count, 10, from_list([0,1,2,3,4,5,6,7,8,9])) == 20),
     ?_assert(foldr(Count, 1000, from_list(lists:seq(0,999))) == 2000),
     ?_assert(foldr(Sum, 0, from_list(lists:seq(0,10))) == 55),
     ?_assert(foldr(List, [], from_list(lists:seq(0,1000)))
 	      == lists:seq(0,1000))
    ].
-endif.


%% @spec (Function, InitialAcc::term(), array()) -> term()
%%    Function = (Index::integer(), Value::term(), Acc::term()) ->
%%               term()
%% @doc Folds the elements of the array right-to-left using the given
%% function and initial accumulator value, skipping default-valued
%% entries. The elements are visited in order from the highest index to
%% the lowest. If `Function' is not a function, the call fails with
%% reason `badarg'.
%%
%% @see foldr/3
%% @see sparse_foldl/3

sparse_foldr(Function, A, #array{size = N, elements = E, default = D})
  when is_function(Function) ->
    if N > 0 ->
	    I = N - 1,
	    sparse_foldr_1(I, E, I, A, Function, D);
       true ->
	    A
    end;
sparse_foldr(_, _, _) ->
    erlang:error(badarg).

%% see foldr/3 for details
%% TODO: this can be optimized

sparse_foldr_1(I, E=?NODEPATTERN(S), Ix, A, F, D) ->
    sparse_foldr_2(I div S + 1, E, Ix, A, F, D, I rem S, S-1);
sparse_foldr_1(_I, E, _Ix, A, _F, _D) when is_integer(E) ->
    A;
sparse_foldr_1(I, E, Ix, A, F, D) ->
    I1 = I+1,
    sparse_foldr_3(I1, E, Ix-I1, A, F, D).

sparse_foldr_2(0, _E, _Ix, A, _F, _D, _R, _R0) ->
    A;
sparse_foldr_2(I, E, Ix, A, F, D, R, R0) ->
    sparse_foldr_2(I-1, E, Ix - R - 1,
	    sparse_foldr_1(R, element(I, E), Ix, A, F, D),
	    F, D, R0, R0).

sparse_foldr_3(0, _T, _Ix, A, _F, _D) ->
    A;
sparse_foldr_3(I, T, Ix, A, F, D) ->
    case element(I, T) of
	D -> sparse_foldr_3(I-1, T, Ix, A, F, D);
	E -> sparse_foldr_3(I-1, T, Ix, F(Ix+I, E, A), F, D)
    end.


-ifdef(EUNIT).
sparse_foldr_test_() ->
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    List = fun (_,X,L) -> [X|L] end,
    [?_assertException(error, badarg, sparse_foldr([], 0, new())),
     ?_assertException(error, badarg, sparse_foldr([], 0, new(10))),
     ?_assert(sparse_foldr(Count, 0, new()) == 0),
     ?_assert(sparse_foldr(Count, 0, new(1)) == 0),
     ?_assert(sparse_foldr(Count, 0, new(10,1)) == 0),
     ?_assert(sparse_foldr(Count, 0, from_list([0,1,2,3,4],0)) == 4),
     ?_assert(sparse_foldr(Count, 0, from_list([0,1,2,3,4,5,6,7,8,9,0],0))
	      == 9),
     ?_assert(sparse_foldr(Count, 0, from_list(lists:seq(0,999),0))
	      == 999),
     ?_assert(sparse_foldr(Sum, 0, from_list(lists:seq(0,10),5)) == 50),
     ?_assert(sparse_foldr(List, [], from_list(lists:seq(0,1000),0))
 	      == lists:seq(1,1000))
    ].
-endif.
