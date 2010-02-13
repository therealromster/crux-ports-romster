%% SQS Amazon REST API for Erlang
%%
%% Created by:  J at jerith@jerith.za.net in may 2, 2007
%%   - first implementation
%% Modified by: Younes Hafri at yerl@club-internet.fr in may 3, 2007
%%   - add start/0
%%   - rewrite downcase/1 as to_lower/1 for speed
%%   - replace most 'if' statements by 'case'
%%   - rewrite build_string_to_sign/1 as string_to_sign/1 for speed
%%   - rewrire url_encode/1 for speed
%%   - add make_request/1
%%   - params sorting isn't needed, remove it
%% Modified by: Younes Hafri at yerl@club-internet.fr in may 4, 2007
%%   - move SQS utility function to "erlsqs_utils.erl"
%%   - speed up processing. No need to sort request params 
%%   - code cleaning

%% clear &&  erlc erlsqs.erl && clear && erl -noshell -s erlsqs test
%% clear &&  erlc +native erlsqs.erl && clear && erl -noshell -s erlsqs test

-module(erlsqs).

%-include_lib("xmerl/include/xmerl.hrl").

-export([start/0, 
         create_queue/1, delete_queue/1, 
         list_queues/0,
         send_message/2, read_message/1]).


-define(SQS_URL,           "http://queue.amazonaws.com").
-define(SQS_VERSION,       "2006-04-01").
-define(AKID,              "Your AKID goes here").
-define(SECRET_KEY,        "Your secret key goes here").
-define(SIGNATURE_VERSION, "1").

%%
start() ->
    crypto:start(),
    application:start(inets).


%%
string_to_sign([{K,V}|L]) ->
    string_to_sign(L, K ++ V).
string_to_sign([{K,V}|L], Str) ->
    string_to_sign(L, Str ++ K ++ V);
string_to_sign([], Str) ->
    Str.


%%
sign_params(Key, Params) ->
    http_base_64:encode(
      binary_to_list(
        crypto:sha_mac(Key, string_to_sign( Params ))
        %crypto:sha_mac(Key, string_to_sign( sort_params(Params) ))
       )
     ).

%%
get_timestamp() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:universaltime(),
    lists:flatten(io_lib:format(
                    "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).



%% make request
make_request([{K,V}|L]) ->
    make_request(L, K ++ "=" ++ V ).
make_request([{K,V}|L], Req) ->
    make_request(L, Req ++ "&" ++ K ++ "=" ++ V );
make_request([], Req) ->
    erlsqs_utils:url_encode(?SQS_URL ++ "/?" ++ Req).


%%
gen_url(Params) ->
    FullParams = [{"AWSAccessKeyId",   ?AKID},
                  {"SignatureVersion", ?SIGNATURE_VERSION},
                  {"Timestamp",        get_timestamp()},
                  {"Version",          ?SQS_VERSION} |
                  Params],
    SignedParams = [{"Signature", sign_params(?SECRET_KEY, FullParams)} |
                    FullParams],
    Req = make_request(SignedParams),
    io:format("~p~n", [Req]),
    Req.


%%
list_queues() ->
    start(),
    http:request( gen_url([{"Action", "ListQueues"}]) ),
    halt(). 

%%
create_queue(QueueName) ->
    QueueName.


%%
delete_queue(QueueName) ->
    QueueName.


%%
send_message(Message, QueueName) ->
    {Message, QueueName}.


%%
read_message(QueueName) ->
    QueueName.
