%% @author Emad El-Haraty <emad@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.
%%
%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

%% @doc HTTP Cookie parsing (RFC 2109, RFC 2965)
%% - parse_cookie/1 shamelessly ripped off mochiweb.
%% - simple_cookie/[3,4] moved off smak_auth_cookie.

-module(ewgi_util_cookie).
-export([parse_cookie/1, test/0]).
-export([cookie_headers/4, cookie_safe_encode/1, cookie_safe_decode/1, get_domains/1]).

-include("ewgi.hrl").

-define(QUOTE, $\").

-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

%% RFC 2616 separators (called tspecials in RFC 2068)
-define(IS_SEPARATOR(C),
        (C < 32 orelse
         C =:= $\s orelse C =:= $\t orelse
         C =:= $( orelse C =:= $) orelse C =:= $< orelse C =:= $> orelse
         C =:= $@ orelse C =:= $, orelse C =:= $; orelse C =:= $: orelse
         C =:= $\\ orelse C =:= $\" orelse C =:= $/ orelse
         C =:= $[ orelse C =:= $] orelse C =:= $? orelse C =:= $= orelse
         C =:= ${ orelse C =:= $})).

-define(COOKIE_DELETE_TRAILER, "; Expires=Thu, 01 Jan 1970 23:00:00 GMT; Max-Age=0").

%% @spec parse_cookie(string()) -> [{K::string(), V::string()}]
%% @doc Parse the contents of a Cookie header field, ignoring cookie
%% attributes, and return a simple property list.
parse_cookie("") -> 
    [];
parse_cookie(Cookie) -> 
    parse_cookie(Cookie, []).

%% @spec test() -> ok
%% @doc Run tests for mochiweb_cookies.
test() ->
    parse_cookie_test(),
    ok.

%% Internal API

parse_cookie([], Acc) ->
    lists:reverse(Acc); 
parse_cookie(String, Acc) -> 
    {{Token, Value}, Rest} = read_pair(String),
    Acc1 = case Token of
               "" ->
                   Acc;
               "$" ++ _ ->
                   Acc;
               _ ->
                   [{Token, Value} | Acc]
           end,
    parse_cookie(Rest, Acc1).

read_pair(String) ->
    {Token, Rest} = read_token(skip_whitespace(String)),
    {Value, Rest1} = read_value(skip_whitespace(Rest)),
    {{Token, Value}, skip_past_separator(Rest1)}.

read_value([$= | Value]) ->
    Value1 = skip_whitespace(Value),
    case Value1 of
        [?QUOTE | _] ->
            read_quoted(Value1);
        _ ->
            read_token(Value1)
    end;
read_value(String) ->
    {"", String}.

read_quoted([?QUOTE | String]) ->
    read_quoted(String, []).

read_quoted([], Acc) ->
    {lists:reverse(Acc), []};
read_quoted([?QUOTE | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
read_quoted([$\\, Any | Rest], Acc) ->
    read_quoted(Rest, [Any | Acc]);
read_quoted([C | Rest], Acc) ->
    read_quoted(Rest, [C | Acc]).
    
skip_whitespace(String) ->
    F = fun (C) -> ?IS_WHITESPACE(C) end,
    lists:dropwhile(F, String).

read_token(String) ->
    F = fun (C) -> not ?IS_SEPARATOR(C) end,
    lists:splitwith(F, String).

skip_past_separator([]) ->    
    [];
skip_past_separator([$; | Rest]) ->
    Rest;
skip_past_separator([$, | Rest]) ->
    Rest;
skip_past_separator([_ | Rest]) ->
    skip_past_separator(Rest).

parse_cookie_test() ->
    %% RFC example
    C1 = "$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"; 
    Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
    Shipping=\"FedEx\"; $Path=\"/acme\"",
    [
     {"Customer","WILE_E_COYOTE"},
     {"Part_Number","Rocket_Launcher_0001"},
     {"Shipping","FedEx"}
    ] = parse_cookie(C1),
    %% Potential edge cases
    [{"foo", "x"}] = parse_cookie("foo=\"\\x\""),
    [] = parse_cookie("="),
    [{"foo", ""}, {"bar", ""}] = parse_cookie("  foo ; bar  "),
    [{"foo", ""}, {"bar", ""}] = parse_cookie("foo=;bar="),
    [{"foo", "\";"}, {"bar", ""}] = parse_cookie("foo = \"\\\";\";bar "),
    [{"foo", "\";bar"}] = parse_cookie("foo=\"\\\";bar").


%%====================================================================
%% Functions for setting the cookie headers in the ewgi_response()
%%====================================================================
cookie_headers(Ctx, CookieName, CookieVal, Sec) ->
    {CurDomain, WildDomain} = get_domains(Ctx),
    SessionHeaders =
	[simple_cookie(CookieName, CookieVal, Sec),
	 simple_cookie(CookieName, CookieVal, Sec, CurDomain),
	 simple_cookie(CookieName, CookieVal, Sec, WildDomain)],
    OldHeaders = ewgi_api:response_headers(Ctx),
    Headers = SessionHeaders ++ remove_cookie_headers(CookieName, OldHeaders, []),
    ewgi_api:response_headers(Headers, Ctx).

-spec get_domains(ewgi_context()) -> {string(), string()}.
get_domains(Ctx) ->
    Cur = case ewgi_api:get_header_value("host", Ctx) of
              undefined ->
                  ewgi_api:server_name(Ctx);
              H ->
                  H
          end,
    Wild = [$.|Cur],
    {Cur, Wild}.

remove_cookie_headers(_, [], Acc) ->
    Acc;
remove_cookie_headers(CookieName, [{"Set-Cookie", [CookieName|_]}|R], Acc) ->
    remove_cookie_headers(CookieName, R, Acc);
remove_cookie_headers(CookieName, [H|R], Acc) ->
    remove_cookie_headers(CookieName, R, [H|Acc]).

-spec simple_cookie(string(), string() | binary(), bool()) -> {string(), iolist()}.
simple_cookie(Name, Val, Sec) when is_binary(Val) ->
    simple_cookie(Name, binary_to_list(Val), Sec);
simple_cookie(Name, Val, Sec) when is_list(Name), is_list(Val) ->
    S = if Sec -> "; Secure"; true -> [] end,
    Exp = case Val of [] -> ?COOKIE_DELETE_TRAILER; _ -> [] end,
    {"Set-Cookie", [Name, $=, Val, "; Path=/", S, Exp]}.

-spec simple_cookie(string(), binary() | string(), bool(), binary() | string()) -> {string(), iolist()}.
simple_cookie(Name, Val, Sec, Domain) when is_binary(Val) ->
    simple_cookie(Name, binary_to_list(Val), Sec, Domain);
simple_cookie(Name, Val, Sec, Domain) when is_binary(Domain) ->
    simple_cookie(Name, Val, Sec, binary_to_list(Domain));
simple_cookie(Name, Val, Sec, Domain) ->
    S = if Sec -> "; Secure"; true -> [] end,
    Exp = case Val of [] -> ?COOKIE_DELETE_TRAILER; _ -> [] end,
    {"Set-Cookie", io_lib:format("~s=~s; Path=/; Domain=~s~s~s", [Name, Val, Domain, S, Exp])}.


-spec cookie_safe_encode(binary()) -> binary().
cookie_safe_encode(Bin) when is_binary(Bin) ->
    Enc = binary_to_list(base64:encode(Bin)),
    list_to_binary(cookie_safe_encode1(Enc, [])).

-spec cookie_safe_encode1(string(), string()) -> string().
cookie_safe_encode1([], Acc) ->
    lists:reverse(Acc);
cookie_safe_encode1([$=|Rest], Acc) ->
    cookie_safe_encode1(Rest, [$~|Acc]);
cookie_safe_encode1([$/|Rest], Acc) ->
    cookie_safe_encode1(Rest, [$_|Acc]);
cookie_safe_encode1([C|Rest], Acc) ->
    cookie_safe_encode1(Rest, [C|Acc]).

-spec cookie_safe_decode(binary() | list()) -> binary().
cookie_safe_decode(Bin) when is_binary(Bin) ->
    cookie_safe_decode(binary_to_list(Bin));
cookie_safe_decode(L) when is_list(L) ->
    Dec = cookie_safe_decode1(L, []),
    base64:decode(Dec).

-spec cookie_safe_decode1(string(), list()) -> list().
cookie_safe_decode1([], Acc) ->
    lists:reverse(Acc);
cookie_safe_decode1([$~|Rest], Acc) ->
    cookie_safe_decode1(Rest, [$=|Acc]);
cookie_safe_decode1([$_|Rest], Acc) ->
    cookie_safe_decode1(Rest, [$/|Acc]);
cookie_safe_decode1([C|Rest], Acc) ->
    cookie_safe_decode1(Rest, [C|Acc]).
