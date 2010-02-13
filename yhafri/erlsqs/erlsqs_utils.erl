%% SQS Amazon REST API for Erlang
%%
%% Created by:  Younes Hafri at yerl@club-internet.fr in may 4, 2007
%%
%%    - move SQS utility functions here
%%

-module(erlsqs_utils).


-export([to_lower/1, url_encode/1, test/0]).


%%
test() ->
    Url = "http://yaws.hyber.org/yman.yaws?page=yaws_api:alpha=22",
    %Url = "http://yaws.hyber.org/yman.yaws?page=yaws_api:alpha=22http://yaws.hyber.org/yman.yaws?page=yaws_api:alpha=22http://yaws.hyber.org/yman.yaws?page=yaws_api:alpha=22",

    {T, R} = timer:tc(erlsqs_utils, url_encode, [Url]),
    io:format("~p ~p~n", [T, R]),

    halt().


%%
to_lower(Str) ->
    to_lower(Str, []).
to_lower([C|Cs], Acc) when C >= $A, C =< $Z ->
    to_lower(Cs, [C+($a-$A)| Acc]);
to_lower([C|Cs], Acc) ->
    to_lower(Cs, [C| Acc]);
to_lower([], Acc) ->
    lists:reverse(Acc).




%% %%
%% sort_params(Params) ->
%%     lists:sort(fun({A,_}, {B,_}) ->
%%                        to_lower(A) < to_lower(B)
%%                end, Params).





%% the following function is taken from yaws
%% Changes: '/' and ':' chars must be encoded, they're not safe.
%% Other set of chars are safe (LOOK TO THE COMMENT), see: 
%%       http://www.permadi.com/tutorial/urlEncoding/
%%       http://www.w3.org/Addressing/URL/url-spec.html
url_encode(L) ->
    url_encode(L, []).

url_encode([H|T], Acc) ->
    if
        H >= $a, $z >= H ->
            url_encode(T, [H | Acc]);
        H >= $A, $Z >= H ->
            url_encode(T, [H | Acc]);
        H >= $0, $9 >= H ->
            url_encode(T, [H | Acc]);
        H == $_; H == $.; H == $-; H == $$; H == $@; H == $+; H == $!; H == $*; H == $"; H == $'; H == $(; H == $); H == $, -> %% I'M A COMMENT
	    url_encode(T, [H | Acc]);
	true ->
	    case integer_to_hex(H) of
		[X, Y] ->
		    url_encode(T, [Y, X, $% | Acc]);
		[X] ->
		    url_encode(T, [X, $0, $% | Acc])
	    end
    end;

url_encode([], Acc) ->
    lists:reverse(Acc).

%% the following function is taken from yaws
old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I<16 ->
    [I-10+$A];
old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).


%% the following function is taken from yaws
integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
	{'EXIT', _} ->
	    old_integer_to_hex(I);
	Int ->
	    Int
    end.

