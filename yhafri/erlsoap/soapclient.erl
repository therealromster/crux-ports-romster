%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is erlsoap-0.2
%%%
%%% The Initial Developer of the Original Code is Ericsson
%%% Telecommunicatie BV. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%----------------------------------------------------------------------
%%% File	: soapclient.erl
%%% Author	: Anton Fedorov <datacompboy@call2ru.com>
%%% Original Author : Erik Reitsma <Erik.Reitsma@etm.ericsson.se>
%%% Description : SOAP client
%%%
%%% Updated : 18 sep 2006 by Anton Fedorov <datacompboy@call2ru.com>
%%% Updated : 21 sep 2006 by Anton Fedorov <datacompboy@call2ru.com>
%%%-------------------------------------------------------------------
-module(soapclient).
-vsn("0.4.1").

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-define(PRINT_MESSAGE(Text,Content),io:format("<~p:~p>(~p): "++Text,[?MODULE,?LINE,self()|Content])).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([invoke/3, invoke/4]).

%%--------------------------------------------------------------------
%% invoke(URL, SoapAction, Parameters) ->
%%			 {ok,Result}
%%
%% URL =		string()
%% SoapAction = atom() | string()
%% Parameters = record()
%%
%% This function performs a SOAP request on the provided
%% URL, with the provided SoapAction and Parameters.
%% Before calling invoke/3 add WSD for URL to soap_registry_server.
%% Parameters should be record || list of records defined in WSD
%% To prepare headers from WSD run
%%	 erlsom:write_hrl(WSD,WSD++".hrl")
%%
invoke(URL, SoapAction, Parameters) ->
    invoke(URL, SoapAction, Parameters, {inets, [{cookies, enabled}], []}).
	

invoke(URL, SoapAction, Parameters, ClientOpts) ->
	?PRINT_MESSAGE("SOAP client request for URL ~p~n",[URL]),
	S = self(),
	P=spawn_link(
	fun() ->
		Res = inner_invoke(URL, SoapAction, Parameters, ClientOpts),
		S!{result,self(),Res}
	end),
	receive
	{result,P,R} ->
		R
	end.

inner_invoke(URL, SoapAction, Parameters, ClientOpts) ->
	{ok, Model} = soap_registry_server:get_xsd(URL),
	Params = if is_list(Parameters) -> Parameters;
				true -> [Parameters]
	end,
	Envelope = {'soap:Envelope',undefined,undefined,{'soap:Body',undefined, Params}},
	{ok, XML} = erlsom:write(Envelope,Model),
	Request = make_request_body(XML),
	SA = if is_list(SoapAction) -> SoapAction;
			is_atom(SoapAction) -> atom_to_list(SoapAction)
	end,
    RequestResponse = 
            case ClientOpts of
                {inets, Opts, Heads} ->
                    inets_request(URL, SA, Request, Opts, Heads);
                {ibrowse, Opts, Heads} ->
                    ibrowse_request(URL, SA, Request, Opts, Heads);
                _ ->
                    ?PRINT_MESSAGE("Undefined client",[]),
                    {}
            end,
    case RequestResponse of
        {ok, 200, _Headers, Body} ->
			?PRINT_MESSAGE("Got 200 reply~n",[]),
			case (catch erlsom:parse(Body,Model)) of
				{'EXIT',Reason} ->
					?PRINT_MESSAGE("Unable to parse reply, reason = ~p~n", [Reason]),
					{error,{undefined,.httpd_util:reason_phrase(501),501,[],[]}};
				{ok, {'soap:Envelope',_,_,{'soap:Body', _, Replies}}} ->
					{ok, Replies};
				Other ->
					{error,{Other,.httpd_util:reason_phrase(501),501,[],[]}}
			end;
        {ok, 500, Headers, Body} ->
            ?PRINT_MESSAGE("Server error header,body ~p~n",[{Headers,Body}]),
            Decoded = case (catch erlsom:parse(Body,Model)) of
                            {'EXIT',_Reason} -> undefined;
                            {'soap:Envelope',undefined,undefined,{'soap:Body',undefined, Replies}} -> Replies;
                            Other -> Other                                                                
			          end,
			{error,{Decoded,
				.httpd_util:reason_phrase(500),
				500,
				Headers,
				Body}};
        {ok, ErrorCode, Headers, Body} ->
            ?PRINT_MESSAGE("Result code not ok but ~p, headers,body=~n~p~n",[ErrorCode,{Headers,Body}]),
            {error,{undefined,.httpd_util:reason_phrase(ErrorCode),ErrorCode,Headers,Body}};
        Other ->
            ?PRINT_MESSAGE("Other HTTP result = ~p while request body was ~p~n", [Other,Request]),
            {error,{undefined,.httpd_util:reason_phrase(502),502,[],Other}}
	end.
%%====================================================================
%% Internal functions
%%====================================================================

inets_request(URL, SoapAction, Request, Options, Headers) ->
    NewHeaders = lists:flatten([{"SOAPAction", SoapAction}] ++ Headers),
    NewOptions = lists:flatten([{cookies, enabled}] ++ Options),
    http:set_options(NewOptions),
    case http:request(post,
			   {URL,NewHeaders,
				"text/xml; charset=utf-8",
				Request},
			   [{http_timeout,20000}],
			   [{sync, true}, {full_result, true}, {body_format, string}]) of
        {ok,{{_HTTP,200,_OK},ResponseHeaders,ResponseBody}} ->
            {ok, 200, ResponseHeaders, ResponseBody};
        {ok,{{_HTTP,500,_Descr},ResponseHeaders,ResponseBody}} ->
            {ok, 500, ResponseHeaders, ResponseBody};
        {ok,{{_HTTP,ErrorCode,_Descr},ResponseHeaders,ResponseBody}} ->
            {ok, ErrorCode, ResponseHeaders, ResponseBody};
		Other ->
            Other
	end.

ibrowse_request(URL, SoapAction, Request, Options, Headers) ->
    Ibrowse = case ibrowse:start() of
                  {ok, _} -> ok;
                  {error, {already_started, _}} -> ok;
                  _ -> error
              end,
    case Ibrowse of
        ok ->
            NewHeaders = lists:flatten([{"SOAPAction", SoapAction}] ++ Headers),
            NewOptions = lists:flatten([{content_type, "text/xml; encoding=utf-8"}] ++ Options),
            case ibrowse:send_req(URL, NewHeaders, post, Request, NewOptions) of
                {ok, Status, ResponseHeaders, ResponseBody} ->
                    {ok, list_to_integer(Status), ResponseHeaders, ResponseBody};
                {error, Reason} ->
                    {error, Reason}
            end;
    	error ->
            {error}
    end.

make_request_body(Content) ->
	"<?xml version=\"1.0\" encoding=\"utf-8\"?>"++Content.
