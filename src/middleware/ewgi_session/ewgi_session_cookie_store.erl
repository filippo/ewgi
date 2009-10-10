%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Cookie-based session store.
%% Based on smak_auth_cookie by Hunter Morris.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ewgi_session_cookie_store).
-author('Hunter Morris <hunter.morris@smarkets.com>').
-author('Davide Marquês <nesrait@gmail.com>').

%% Session Store API
-export([load_session/2, store_session/2, delete_session/2]).

%% Usage examples
-export([create_example/1, delete_example/1]).

-import(ewgi_util_cookie, [cookie_headers/4, cookie_safe_encode/1, cookie_safe_decode/1]).

-include("ewgi.hrl").

-define(DEFAULT_COOKIE_NAME, "session_id").
-define(ENCODER, ewgi_util_crypto).
-define(MAX_LENGTH, 4096).

%%====================================================================
%% Session Store API
%%====================================================================
-spec load_session(ewgi_context(), list()) -> ewgi_context().
load_session(Ctx, [CookieName, _SecureCookie, Timeout, IncludeIp, Key]=Args) ->
    case ewgi_api:get_header_value("cookie", Ctx) of
	undefined ->
	    ewgi_session:new_session(Ctx);
	Cookies ->
	    CookieValues = ewgi_util_cookie:parse_cookie(Cookies),
	    case proplists:get_value(CookieName, CookieValues) of
		undefined ->
		    ewgi_session:new_session(Ctx);
		SessionCookie ->
		    case decode_cookie_contents(SessionCookie, Key) of
			{error,_} = _Error ->
			    %% DISPLAY ERROR!?
			    ewgi_session:new_session(Ctx);
			Session ->
			    case ewgi_session:init_session(Ctx, Session, Timeout, IncludeIp) of
				invalid_session ->
				    %% DISPLAY ERROR!?
				    Ctx_2 = ?MODULE:delete_session(Ctx, Args),
				    ewgi_session:new_session(Ctx_2);
				Ctx_2 ->
				    Ctx_2
			    end
		    end
	    end
    end.

-spec store_session(ewgi_context(), list()) -> ewgi_context().
store_session(Ctx, [CookieName, SecureCookie, _Timeout, IncludeIp, Key]) ->
    Updated = ewgi_session:session_updated(Ctx),
    if Updated ->
	    Session = ewgi_session:get_session(Ctx, IncludeIp),
	    case ?ENCODER:encode(Key, term_to_binary(Session), ?MAX_LENGTH) of
		{error, _} = _Error ->
		    %% TODO: report this? To whom?
		    Ctx;
		EncryptedVal ->
		    CookieVal = cookie_safe_encode(EncryptedVal),
		    cookie_headers(Ctx, CookieName, CookieVal, SecureCookie)
	    end;
       true -> %% nothing to do!
	    Ctx
    end.

delete_session(Ctx,  [CookieName, SecureCookie, _Timeout, _IncludeIp, _Key]) ->
    cookie_headers(Ctx, CookieName, [], SecureCookie).

%%====================================================================
%% example functions on how to use the session middleware
%%====================================================================
-define(COOKIE_SIGNING_KEY, <<"ABCDEFGHIJKLMNOP">>).
-define(SECURE_COOKIE, false).
-define(INCLUDE_IP, true).
-define(SESSION_TIMEOUT, 15 * 60 * 1000). %% 15 minutes
-define(COOKIE_STORE_ARGS, [
			    "cookie_session_id",
			    ?SECURE_COOKIE,
			    ?SESSION_TIMEOUT,
			    ?INCLUDE_IP,
			    ?COOKIE_SIGNING_KEY
			   ]).

create_example(Ctx) ->
    SessionApp = fun ewgi_session:session_create_app/1,
    Ctx1 = ?MODULE:load_session(Ctx, ?COOKIE_STORE_ARGS),
    Ctx2 = SessionApp(Ctx1),
    ?MODULE:store_session(Ctx2, ?COOKIE_STORE_ARGS).

delete_example(Ctx) ->
    SessionApp = fun ewgi_session:session_delete_app/1,
    Ctx1 = ?MODULE:load_session(Ctx, ?COOKIE_STORE_ARGS),
    Ctx2 = SessionApp(Ctx1),
    ?MODULE:store_session(Ctx2, ?COOKIE_STORE_ARGS).

%%====================================================================
%% Internal functions
%%====================================================================
-spec decode_cookie_contents(string(), list()) -> any() | {'error', any()}.
decode_cookie_contents(Cookie0, Key) ->
    case cookie_safe_decode(Cookie0) of
        <<>> ->
	    {error, {decode_cookie_contents, no_data}};
        Cookie when is_binary(Cookie) ->
	    case ?ENCODER:decode(Key, Cookie) of
		{error, _} = Error ->
		    Error;
		Content when is_binary(Content) ->
		    case (catch(binary_to_term(Content))) of
			{'EXIT', Error} ->
			    {error, {decode_cookie_contents, Error}};
			Session ->
			    Session
		    end
	    end
    end.
