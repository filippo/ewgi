%%%-------------------------------------------------------------------
%%% File    : ewgi_api.erl
%%% Authors : Filippo Pacini <filippo.pacini@gmail.com>
%%%           Hunter Morris <huntermorris@gmail.com>
%%% License :
%%% The contents of this file are subject to the Mozilla Public
%%% License Version 1.1 (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of
%%% the License at http://www.mozilla.org/MPL/
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and
%%% limitations under the License.
%%% The Initial Developer of the Original Code is S.G. Consulting
%%% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
%%% 2007 S.G. Consulting srl. All Rights Reserved.
%%%
%%% @doc 
%%% <p>ewgi API. Defines a low level CGI like API.</p>
%%%
%%% @end
%%%
%%% Created : 10 Oct 2007 by Filippo Pacini <filippo.pacini@gmail.com>
%%%-------------------------------------------------------------------
-module(ewgi_api).

-include_lib("ewgi.hrl").

%% Record helpers
-export([empty_request/0, empty_response/0, context/2, request/2, response/2,
         request/1, response/1]).

-export([response_headers/1, response_message_body/1, response_status/1,
         response_error/1]).

-export([response_headers/2, response_message_body/2, response_status/2,
         response_error/2]).

%% Request 'get' methods
-export([auth_type/1, content_length/1, content_type/1, gateway_interface/1,
         path_info/1, path_translated/1, query_string/1, remote_addr/1,
         remote_host/1, remote_ident/1, remote_user/1, remote_user_data/1,
         request_method/1, script_name/1, server_name/1, server_port/1,
         server_protocol/1, server_software/1]).

%% Request 'set' methods
-export([auth_type/2, content_length/2, content_type/2, gateway_interface/2,
         path_info/2, path_translated/2, query_string/2, remote_addr/2,
         remote_host/2, remote_ident/2, remote_user/2, remote_user_data/2,
         request_method/2, script_name/2, server_name/2, server_port/2,
         server_protocol/2, server_software/2]).

%% Additional request methods
-export([get_header_value/2, set_header/3, insert_header/3,
         get_all_headers/1, read_input/1, read_input/3, read_input_string/2,
         write_error/2, url_scheme/1, version/1, get_all_data/1, find_data/2,
         find_data/3, store_data/3]).

%% Server methods
-export([server_request_foldl/4]).

%% Utility methods
-export([parse_qs/1, parse_post/1, urlencode/1, quote/1, normalize_header/1,
         unquote_path/1, path_components/3]).

%%====================================================================
%% API
%%====================================================================
-spec empty_request() -> #ewgi_request{}.
empty_request() ->
    #ewgi_request{}.

-spec empty_response() -> #ewgi_response{}.
empty_response() ->
    #ewgi_response{}.

-spec context(#ewgi_request{}, #ewgi_response{}) -> #ewgi_context{}.
context(Request, Response) when is_record(Request, ewgi_request),
                                is_record(Response, ewgi_response) ->
    #ewgi_context{request=Request, response=Response}.

-spec request(#ewgi_request{}, #ewgi_context{}) -> #ewgi_context{}.
request(Req, Ctx) when is_record(Req, ewgi_request),
                       is_record(Ctx, ewgi_context) ->
    Ctx#ewgi_context{request=Req}.

-spec response(#ewgi_response{}, #ewgi_context{}) -> #ewgi_context{}.
response(Rsp, Ctx) when is_record(Rsp, ewgi_response),
                        is_record(Ctx, ewgi_context) ->
    Ctx#ewgi_context{response=Rsp}.

-spec response(#ewgi_context{}) -> #ewgi_response{}.
response(#ewgi_context{response=R}) ->
    R.

-spec request(#ewgi_context{}) -> #ewgi_request{}.
request(#ewgi_context{request=R}) ->
    R.

-spec response_headers(#ewgi_context{}) -> ewgi_header_list().
response_headers(#ewgi_context{response=#ewgi_response{headers=V}}) ->
    V.

-spec response_status(#ewgi_context{}) -> ewgi_status().
response_status(#ewgi_context{response=#ewgi_response{status=V}}) ->
    V.

-spec response_message_body(#ewgi_context{}) -> ewgi_message_body().
response_message_body(#ewgi_context{response=#ewgi_response{message_body=V}}) ->
    V.

-spec response_error(#ewgi_context{}) -> any().
response_error(#ewgi_context{response=#ewgi_response{err=V}}) ->
    V.

-spec response_headers(ewgi_header_list(), #ewgi_context{}) -> #ewgi_context{}.
response_headers(V, #ewgi_context{response=Rsp0}=Ctx0) ->
    Rsp = Rsp0#ewgi_response{headers=V},
    Ctx = Ctx0#ewgi_context{response=Rsp},
    Ctx.

-spec response_status(ewgi_status(), #ewgi_context{}) -> #ewgi_context{}.
response_status(V, #ewgi_context{response=Rsp0}=Ctx0) ->
    Rsp = Rsp0#ewgi_response{status=V},
    Ctx = Ctx0#ewgi_context{response=Rsp},
    Ctx.

-spec response_message_body(ewgi_message_body(), #ewgi_context{}) -> #ewgi_context{}.
response_message_body(V, #ewgi_context{response=Rsp0}=Ctx0) ->
    Rsp = Rsp0#ewgi_response{message_body=V},
    Ctx = Ctx0#ewgi_context{response=Rsp},
    Ctx.

-spec response_error(any(), #ewgi_context{}) -> #ewgi_context{}.
response_error(V, #ewgi_context{response=Rsp0}=Ctx0) ->
    Rsp = Rsp0#ewgi_response{err=V},
    Ctx = Ctx0#ewgi_context{response=Rsp},
    Ctx.

-spec auth_type(#ewgi_context{}) -> ewgi_val().
auth_type(#ewgi_context{request=#ewgi_request{auth_type=V}}) ->
    V.

-spec content_length(#ewgi_context{}) -> non_neg_integer().
content_length(#ewgi_context{request=#ewgi_request{content_length=V}}) ->
    V.

-spec content_type(#ewgi_context{}) -> ewgi_val().
content_type(#ewgi_context{request=#ewgi_request{content_type=V}}) ->
    V.

-spec gateway_interface(#ewgi_context{}) -> ewgi_val().
gateway_interface(#ewgi_context{request=#ewgi_request{gateway_interface=V}}) ->
    V.

-spec path_info(#ewgi_context{}) -> ewgi_val().
path_info(#ewgi_context{request=#ewgi_request{path_info=V}}) ->
    V.

-spec path_translated(#ewgi_context{}) -> ewgi_val().
path_translated(#ewgi_context{request=#ewgi_request{path_translated=V}}) ->
    V.

-spec query_string(#ewgi_context{}) -> ewgi_val().
query_string(#ewgi_context{request=#ewgi_request{query_string=V}}) ->
    V.

-spec remote_addr(#ewgi_context{}) -> ewgi_val().
remote_addr(#ewgi_context{request=#ewgi_request{remote_addr=V}}) ->
    V.

-spec remote_host(#ewgi_context{}) -> ewgi_val().
remote_host(#ewgi_context{request=#ewgi_request{remote_host=V}}) ->
    V.

-spec remote_ident(#ewgi_context{}) -> ewgi_val().
remote_ident(#ewgi_context{request=#ewgi_request{remote_ident=V}}) ->
    V.

-spec remote_user(#ewgi_context{}) -> ewgi_val().
remote_user(#ewgi_context{request=#ewgi_request{remote_user=V}}) ->
    V.

-spec remote_user_data(#ewgi_context{}) -> ewgi_val().
remote_user_data(#ewgi_context{request=#ewgi_request{remote_user_data=V}}) ->
    V.

-spec request_method(#ewgi_context{}) -> ewgi_request_method().
request_method(#ewgi_context{request=#ewgi_request{request_method=V}}) ->
    V.

-spec script_name(#ewgi_context{}) -> ewgi_val().
script_name(#ewgi_context{request=#ewgi_request{script_name=V}}) ->
    V.

-spec server_name(#ewgi_context{}) -> ewgi_val().
server_name(#ewgi_context{request=#ewgi_request{server_name=V}}) ->
    V.

-spec server_port(#ewgi_context{}) -> ewgi_val().
server_port(#ewgi_context{request=#ewgi_request{server_port=V}}) ->
    V.

-spec server_protocol(#ewgi_context{}) -> ewgi_val().
server_protocol(#ewgi_context{request=#ewgi_request{server_protocol=V}}) ->
    V.

-spec server_software(#ewgi_context{}) -> ewgi_val().
server_software(#ewgi_context{request=#ewgi_request{server_software=V}}) ->
    V.

-spec get_header_value(string(), #ewgi_context{}) -> ewgi_header_val().
get_header_value(Hdr0, Ctx) when is_list(Hdr0) ->
    Hdr = string:to_lower(Hdr0),
    get_header1(Hdr, Ctx).

get_header1("accept", #ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{http_accept=V}}}) ->
    V;
get_header1("cookie", #ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{http_cookie=V}}}) ->
    V;
get_header1("host", #ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{http_host=V}}}) ->
    V;
get_header1("if-modified-since", #ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{http_if_modified_since=V}}}) ->
    V;
get_header1("user-agent", #ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{http_user_agent=V}}}) ->
    V;
get_header1("x-http-method-override", #ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{http_x_http_method_override=V}}}) ->
    V;
get_header1(Hdr, #ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{other=D}}}) ->
    case gb_trees:lookup(Hdr, D) of
        {value, V} when is_list(V) ->
            {_, V1} = lists:unzip(V),
            string:join(V1, ", ");
        none ->
            undefined
    end.

insert_header(K0, V, #ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{other=D0}=H0}=R0}=Ctx0) ->
    K = string:to_lower(K0),
    D = t_insert_header(K, {K0, V}, D0),
    H = H0#ewgi_http_headers{other=D},
    R = R0#ewgi_request{http_headers=H},
    Ctx = Ctx0#ewgi_context{request=R},
    Ctx.

set_header(K0, V, #ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{other=D0}=H0}=R0}=Ctx0) ->
    K = string:to_lower(K0),
    D = t_enter_header(K, {K0, V}, D0),
    H = H0#ewgi_http_headers{other=D},
    R = R0#ewgi_request{http_headers=H},
    Ctx = Ctx0#ewgi_context{request=R},
    Ctx.

auth_type(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{auth_type=V},
    Ctx0#ewgi_context{request=Req}.

content_length(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{content_length=V},
    Ctx0#ewgi_context{request=Req}.

content_type(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{content_type=V},
    Ctx0#ewgi_context{request=Req}.

gateway_interface(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{gateway_interface=V},
    Ctx0#ewgi_context{request=Req}.

path_info(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{path_info=V},
    Ctx0#ewgi_context{request=Req}.

path_translated(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{path_translated=V},
    Ctx0#ewgi_context{request=Req}.

query_string(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{query_string=V},
    Ctx0#ewgi_context{request=Req}.

remote_addr(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{remote_addr=V},
    Ctx0#ewgi_context{request=Req}.

remote_host(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{remote_host=V},
    Ctx0#ewgi_context{request=Req}.

remote_ident(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{remote_ident=V},
    Ctx0#ewgi_context{request=Req}.

remote_user(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{remote_user=V},
    Ctx0#ewgi_context{request=Req}.

remote_user_data(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{remote_user_data=V},
    Ctx0#ewgi_context{request=Req}.

request_method(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{request_method=V},
    Ctx0#ewgi_context{request=Req}.

script_name(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{script_name=V},
    Ctx0#ewgi_context{request=Req}.

server_name(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{server_name=V},
    Ctx0#ewgi_context{request=Req}.

server_port(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{server_port=V},
    Ctx0#ewgi_context{request=Req}.

server_protocol(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{server_protocol=V},
    Ctx0#ewgi_context{request=Req}.

server_software(V, #ewgi_context{request=Req0}=Ctx0) ->
    Req = Req0#ewgi_request{server_software=V},
    Ctx0#ewgi_context{request=Req}.

get_all_headers(#ewgi_context{request=#ewgi_request{http_headers=#ewgi_http_headers{other=HDict}=H}}) ->
    F = fun({K, L}, Acc0) ->
                lists:foldl(fun (V, Acc) -> [{K, V}|Acc] end, Acc0, L)
        end,
    Acc = lists:foldl(F, [], gb_trees:to_list(HDict)),
    [{"accept", H#ewgi_http_headers.http_accept},
     {"cookie", H#ewgi_http_headers.http_cookie},
     {"host", H#ewgi_http_headers.http_host},
     {"if-modified-since", H#ewgi_http_headers.http_if_modified_since},
     {"user-agent", H#ewgi_http_headers.http_user_agent},
     {"x-http-method-override", H#ewgi_http_headers.http_x_http_method_override}|Acc].

-spec read_input(#ewgi_context{}) -> ewgi_ri_callback() | 'undefined'.
read_input(#ewgi_context{request=#ewgi_request{ewgi=#ewgi_spec{read_input=F}}}) ->
    F.

-spec read_input(ewgi_ri_callback(), non_neg_integer(), #ewgi_context{}) -> ewgi_ri_callback() | 'undefined'.
read_input(Callback, Length, #ewgi_context{request=#ewgi_request{ewgi=#ewgi_spec{read_input=undefined}}})
  when is_function(Callback, 1),
       is_integer(Length),
       Length >= 0 ->
    undefined;
read_input(Callback, Length, #ewgi_context{request=#ewgi_request{ewgi=#ewgi_spec{read_input=F}}})
  when is_function(F, 2),
       is_function(Callback, 1),
       is_integer(Length),
       Length >= 0 ->
    F(Callback, Length).

%% @spec read_input_string(non_neg_integer(), ewgi_context()) -> string() | {error, no_input}
%% @doc Reads the client message body into a string from the EWGI context.
-spec read_input_string(non_neg_integer(), #ewgi_context{}) -> [byte()] | {'error', 'no_input'}.
read_input_string(L, Ctx) when is_integer(L), L >= 0 ->
    case read_input(read_input_string_cb([]), L, Ctx) of
        undefined ->
            {error, no_input};
        Iol ->
            Bin = iolist_to_binary(Iol),
            binary_to_list(Bin)
    end.

-spec read_input_string_cb(list()) -> ewgi_ri_callback().
read_input_string_cb(Acc) ->
    F = fun(eof) ->
                lists:reverse(Acc);
           ({data, B}) ->
                read_input_string_cb([B|Acc])
        end,
    F.

write_error(Msg, #ewgi_context{request=#ewgi_request{ewgi=#ewgi_spec{write_error=F}}}) ->
    F(Msg).

url_scheme(#ewgi_context{request=#ewgi_request{ewgi=#ewgi_spec{url_scheme=V}}}) ->
    V.

version(#ewgi_context{request=#ewgi_request{ewgi=#ewgi_spec{version=V}}}) ->
    V.

get_all_data(#ewgi_context{request=#ewgi_request{ewgi=#ewgi_spec{data=D}}}) ->
    D.

find_data(Key, Ctx) ->
    find_data(Key, Ctx, undefined).

find_data(Key, #ewgi_context{request=#ewgi_request{ewgi=#ewgi_spec{data=D}}}, Default) ->
    case gb_trees:lookup(Key, D) of
        {value, V} ->
            V;
        none ->
            Default
    end.

store_data(Key, Val, #ewgi_context{request=#ewgi_request{ewgi=#ewgi_spec{data=D0}=E}=Req}=Ctx) ->
    D = gb_trees:enter(Key, Val, D0),
    Ctx#ewgi_context{request=Req#ewgi_request{ewgi=E#ewgi_spec{data=D}}}.

%%--------------------------------------------------------------------
%% @spec parse_qs(string()|binary()) -> [proplist()]
%%
%% @doc Parse a query string. Calls parse_data to do the job.
%% @end
%%--------------------------------------------------------------------
parse_qs(ToParse) ->
    parse_data(ToParse).

%%--------------------------------------------------------------------
%% @spec parse_post(string()|binary()) -> [proplist()]
%%
%% @doc Parse application/x-www-form-urlencoded data. 
%% Calls parse_data to do the job.
%% @end
%%--------------------------------------------------------------------
parse_post(ToParse) ->
    parse_data(ToParse).

%%--------------------------------------------------------------------
%% @spec parse_data(string()|binary()) -> [proplist()]
%%
%% @doc Parse a query string or application/x-www-form-urlencoded data.
%% @end
%%--------------------------------------------------------------------
parse_data(undefined) ->
    [];
parse_data(Binary) when is_binary(Binary) ->
    parse_data(binary_to_list(Binary), []);
parse_data(String) ->
    parse_data(String, []).

parse_data([], Acc) ->
    lists:reverse(Acc);
parse_data(String, Acc) ->
    {{Key, Val}, Rest} = parse_kv(String),
    parse_data(Rest, [{Key, Val} | Acc]).


%%--------------------------------------------------------------------
%% @spec urlencode(proplist()) -> string()
%%
%% @doc URL encodes a proplist of parameters.
%% @end
%%--------------------------------------------------------------------
-spec urlencode(ewgi_proplist()) -> string().
urlencode(Props) ->
    QuotedL = [[quote(K), $=, quote(V)] || {K, V} <- Props],
    lists:flatten(join(QuotedL, $&)).

%%--------------------------------------------------------------------
%% @spec quote(term()) -> string()
%%
%% @doc URL encodes the given term.
%% @end
%%--------------------------------------------------------------------
-spec quote(ewgi_propval()) -> string().
quote(Term) when is_atom(Term) ->
    quote(atom_to_list(Term));
quote(Term) when is_integer(Term) ->
    quote(integer_to_list(Term));
quote(Term) when is_binary(Term) ->
    quote(binary_to_list(Term));
quote(Term) when is_list(Term) ->
    quote(Term, []).

-spec quote(string(), string()) -> string().
quote([], Acc) ->
    lists:reverse(Acc);
%% low alpha chars
quote([H|Rest], Acc) when H >= $a, H =< $z ->
    quote(Rest, [H|Acc]);
%% hialpha chars 
quote([H|Rest], Acc) when H >= $A, H =< $Z ->
    quote(Rest, [H|Acc]);
%% digit chars
quote([H|Rest], Acc) when H >= $0, H =< $9 ->
    quote(Rest, [H|Acc]);
%% safe chars
quote([H|Rest], Acc) when H =:= $-; H=:=$.; H=:=$_; H=:=$~ ->
    quote(Rest, [H|Acc]);
%% space
quote([$\s|Rest], Acc) ->
    quote(Rest, [$+ | Acc]);
%% other characters (convert to hex)
quote([H|Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<H>>,
    quote(Rest, [to_hex(Lo), to_hex(Hi), $\% | Acc]).


%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec parse_kv(String::string()) -> parsed()|{error, Reason}
%%
%% @type parsed() = {ok, proplist(), Rest::string()}
%%
%% @doc Parser for kv pairs found in query strings and body data.
%% returns the first proplist parsed from String or an error. 
%% @end
%%--------------------------------------------------------------------
-spec parse_kv(string()) -> {{string(), string()}, string()}.
parse_kv(String) ->
    P = and_parser([until(fun is_equal/1), until(fun is_amp/1)]),
    {ok, [K, V], Rest} = P(String),
    {{unquote(K), unquote(V)}, Rest}.

%%--------------------------------------------------------------------
%% @spec and_parser(Rules::rules()) -> parsed()|{error, Reason}
%%
%% @type rules() = [rule()]
%%       rule()  = function(template()).
%%
%% @doc and_parser of Rules. 
%% Applies each Rule in sequence to the Template passed. 
%% If a rule fails returns an error.
%% @end
%%--------------------------------------------------------------------
-type parser() :: fun((list()) -> {'ok', list(), list()}).
-spec and_parser([parser()]) -> parser().
and_parser(Rules) ->
    fun(Tmpl) ->
	    and_parser(Rules, Tmpl, [])
    end.

-spec and_parser(list(), list(), list()) -> {'ok', list(), list()}.
and_parser([], Tmpl, SoFar) ->
    {ok, lists:reverse(SoFar), Tmpl};
and_parser([Rule|T], Tmpl, SoFar) ->
    {ok, Tok, Rest} = Rule(Tmpl),
    and_parser(T, Rest, [Tok|SoFar]).

%%--------------------------------------------------------------------
%% @spec until(predicate()) -> parsed()|{error, Reason}
%%
%% @type predicate() = function(template()).
%%
%% @doc until predicate P: 
%% output what it gets until P(H) is true.
%% @end
%%--------------------------------------------------------------------
-type predicate() :: fun((list()) -> {'true', list()} | 'false').
-spec until(predicate()) -> parser().
until(P) ->
    fun (Tmpl) -> until(P, Tmpl, []) end.

-spec until(predicate(), list(), list()) -> {'ok', list(), list()}.
until(_P, [], Parsed) -> %% end of string so end parsing
    {ok, lists:reverse(Parsed), []};
until(P, String, Parsed) ->
    case P(String) of
	{true, Rest} ->
	    {ok, lists:reverse(Parsed), Rest};
	_ ->
            [H|Rest] = String,
	    until(P, Rest, [H|Parsed])
    end.

%%--------------------------------------------------------------------
%% @spec is_equal(string()) -> bool()
%%
%% @doc Match = character at the head of string.
%% @end
%%--------------------------------------------------------------------
-spec is_equal(string()) -> bool().
is_equal([$=|Rest]) ->
    {true, Rest};
is_equal(_) ->
    false.

%%--------------------------------------------------------------------
%% @spec is_amp(string()) -> bool()
%%
%% @doc Match &amp; character or &amp;amp; entity at the beginning of string.
%% @end
%%--------------------------------------------------------------------
-spec is_amp(string()) -> bool().
is_amp("&amp;"++Rest) ->
    {true, Rest};
is_amp([$&|Rest]) ->
    {true, Rest};
is_amp(_) ->
    false.


%%--------------------------------------------------------------------
%% @spec unquote(string()) -> string()
%%
%% @doc URL decodes the given term. 
%% Used to parse query strings and application/x-www-form-urlencoded data. 
%% @end
%%--------------------------------------------------------------------
unquote(Val) when is_binary(Val) ->
    unquote(binary_to_list(Val), []);
unquote(Val) ->
    unquote(Val, []).

unquote([], Acc) ->
    lists:reverse(Acc);
unquote([37, Hi, Lo|Rest], Acc) -> % match %Hex 
    unquote(Rest, [(from_hex(Lo) bor (from_hex(Hi) bsl 4))|Acc]);
unquote([$+|Rest], Acc) ->
    unquote(Rest, [$\s|Acc]);
unquote([H|Rest], Acc) ->
    unquote(Rest, [H|Acc]).

%%--------------------------------------------------------------------
%% @spec to_hex(char()) -> hex()
%%
%% @doc convert char to hex code.
%% @end
%%--------------------------------------------------------------------
-spec to_hex(0..16) -> byte().
to_hex(C) when C >= 0, C < 10 -> $0 + C;
to_hex(C) when C >= 0, C < 16 -> $A + (C - 10).

%%--------------------------------------------------------------------
%% @spec from_hex(hex()) -> char()
%%
%% @doc Used to get char from hex code.
%% @end
%%--------------------------------------------------------------------
-spec from_hex(byte()) -> 0..16.
from_hex(C) when C >= $0, C =< $9 -> C - $0;
from_hex(C) when C >= $a, C =< $f -> C - $a + 10;
from_hex(C) when C >= $A, C =< $F -> C - $A + 10.

%%--------------------------------------------------------------------
%% @spec join([string()], Sep::string()) -> string()
%%
%% @doc Joins a list of elements using a separator. 
%% The result is reversed for efficiency.
%% @end
%%--------------------------------------------------------------------
-spec join([string()], string() | char()) -> string().
join(Strings, Sep) ->
    join(Strings, Sep, []).

-spec join([string()], string() | char(), list()) -> string().
join([], _Sep, _Acc) ->
    [];
join([Last], _Sep, Acc) ->
    [Last|Acc];
join([H|Rest], Sep, Acc) ->
    join(Rest, Sep, [Sep, H|Acc]).

-spec nhdr(atom() | binary() | string()) -> string().
nhdr(L) when is_atom(L) ->
    nhdr(atom_to_list(L));
nhdr(L) when is_binary(L) ->
    nhdr(binary_to_list(L));
nhdr(L) when is_list(L) ->
    string:strip(string:to_lower(L)).

normalize_header({K, V}) ->
    {nhdr(K), string:strip(V)}.

%% http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2
%% and
%% http://www.ietf.org/rfc/rfc2396.txt, sec 2.4.2
unquote_path(Path) ->
    PathComponents = [unquote(X) || X <- path_components(Path, [], [])],
    lists:flatten(join(PathComponents, "%2F")).

path_components([], Piece, Acc) ->
    [lists:reverse(Piece)|Acc];
path_components("%2f" ++ Rest, Piece, Acc) ->
    path_components(Rest, [], [lists:reverse(Piece)|Acc]);
path_components("%2F" ++ Rest, Piece, Acc) ->
    path_components(Rest, [], [lists:reverse(Piece)|Acc]);
path_components([C|Rest], Piece, Acc) ->
    path_components(Rest, [C|Piece], Acc).

%% These two macros make dealing with the request records simpler
-define(EWGI_RECSET(R, K), fun(Rec, V) -> Rec#R{K=V} end).
-define(EWGI_FIELD_PAIR(R, K), {K, ?EWGI_RECSET(R, K)}).

-define(EWGI_SPEC_FIELDS, [?EWGI_FIELD_PAIR(ewgi_spec, read_input),
                           ?EWGI_FIELD_PAIR(ewgi_spec, write_error),
                           ?EWGI_FIELD_PAIR(ewgi_spec, url_scheme),
                           ?EWGI_FIELD_PAIR(ewgi_spec, version),
                           ?EWGI_FIELD_PAIR(ewgi_spec, data)]).

-define(EWGI_HTTP_HEADER_FIELDS, [?EWGI_FIELD_PAIR(ewgi_http_headers, http_accept),
                                  ?EWGI_FIELD_PAIR(ewgi_http_headers, http_cookie),
                                  ?EWGI_FIELD_PAIR(ewgi_http_headers, http_host),
                                  ?EWGI_FIELD_PAIR(ewgi_http_headers, http_if_modified_since),
                                  ?EWGI_FIELD_PAIR(ewgi_http_headers, http_user_agent),
                                  ?EWGI_FIELD_PAIR(ewgi_http_headers, http_x_http_method_override),
                                  ?EWGI_FIELD_PAIR(ewgi_http_headers, other)]).

-define(EWGI_REQUEST_FIELDS, [?EWGI_FIELD_PAIR(ewgi_request, auth_type),
                              ?EWGI_FIELD_PAIR(ewgi_request, content_length),
                              ?EWGI_FIELD_PAIR(ewgi_request, content_type),
                              ?EWGI_FIELD_PAIR(ewgi_request, ewgi),
                              ?EWGI_FIELD_PAIR(ewgi_request, gateway_interface),
                              ?EWGI_FIELD_PAIR(ewgi_request, http_headers),
                              ?EWGI_FIELD_PAIR(ewgi_request, path_info),
                              ?EWGI_FIELD_PAIR(ewgi_request, path_translated),
                              ?EWGI_FIELD_PAIR(ewgi_request, query_string),
                              ?EWGI_FIELD_PAIR(ewgi_request, remote_addr),
                              ?EWGI_FIELD_PAIR(ewgi_request, remote_host),
                              ?EWGI_FIELD_PAIR(ewgi_request, remote_ident),
                              ?EWGI_FIELD_PAIR(ewgi_request, remote_user),
                              ?EWGI_FIELD_PAIR(ewgi_request, remote_user_data),
                              ?EWGI_FIELD_PAIR(ewgi_request, request_method),
                              ?EWGI_FIELD_PAIR(ewgi_request, script_name),
                              ?EWGI_FIELD_PAIR(ewgi_request, server_name),
                              ?EWGI_FIELD_PAIR(ewgi_request, server_port),
                              ?EWGI_FIELD_PAIR(ewgi_request, server_protocol),
                              ?EWGI_FIELD_PAIR(ewgi_request, server_software)]).

server_request_foldl(Req0, ParseFun0, ParseEwgiFun, ParseHttpFun) ->
    ParseFun = fun(ewgi, Req) ->
                       request_foldl(Req, ParseEwgiFun, #ewgi_spec{}, ?EWGI_SPEC_FIELDS);
                  (http_headers, Req) ->
                       request_foldl(Req, ParseHttpFun, #ewgi_http_headers{}, ?EWGI_HTTP_HEADER_FIELDS);
                  (Field, Req) ->
                       ParseFun0(Field, Req)
               end,
    request_foldl(Req0, ParseFun, #ewgi_request{}, ?EWGI_REQUEST_FIELDS).

request_foldl(Req, ParseFun, EmptyRec, Fields) ->
    lists:foldl(fun({Field, F}, Rec) ->
                        case ParseFun(Field, Req) of
                            undefined ->
                                Rec;
                            V ->
                                F(Rec, V)
                        end
                end, EmptyRec, Fields).

t_lookup_default(K, T, Default) ->
    case gb_trees:lookup(K, T) of
        {value, V} ->
            V;
        none ->
            Default
    end.

t_insert_header(K, Pair, T) ->
    gb_trees:enter(K, lists:reverse([Pair|lists:reverse(t_lookup_default(K, T, []))]), T).

t_enter_header(K, Pair, T) ->
    gb_trees:enter(K, Pair, T).
