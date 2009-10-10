%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Davide Marquês <nesrait@gmail.com>
%%
%% @doc Interface for server-side session stores.
%%
%% This store serves as an interface for stores that manage
%% client sessions on the server side using session_id's to
%% between differenciate the sessions.
%%
%% Currently cookies are needed for the client to hold the session_id.
%% BUT/TODO: we could also encode the session_id on the urls so it might
%% be a good idea to swap cookie_headers/4 for a inject_session_data_on_response/?
%% function defined on the ewgi_session module.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ewgi_session_server_store).
-author('Davide Marquês <nesrait@gmail.com>').

%% Session Store API
-export([load_session/2, delete_session/2, store_session/2]).

%% Usage examples
-export([create_example/1, delete_example/1]).

-import(ewgi_util_cookie, [cookie_headers/4, cookie_safe_encode/1, cookie_safe_decode/1]).

-include("ewgi.hrl").

-define(SESSION_ID, "ewgi.session_server_store.session_id").
-define(SESSION_SERVER_MODULE, ewgi_session_server).

%%====================================================================
%% Session Store API
%%====================================================================
load_session(Ctx, [ServerId, CookieName, _SecureCookie, Timeout, IncludeIp] = StoreArgs) ->
    case ewgi_api:get_header_value("cookie", Ctx) of
	undefined ->
	    ewgi_session2:new_session(Ctx);
	Cookies ->
	    CookieValues = ewgi_util_cookie:parse_cookie(Cookies),
	    case proplists:get_value(CookieName, CookieValues) of
		undefined ->
		    ewgi_session:new_session(Ctx);
		SidB64 ->
		    BinSid = cookie_safe_decode(SidB64),
		    case (catch(binary_to_term(BinSid))) of
			Sid when is_list(Sid) ->
			    Ctx1 = ewgi_api:store_data(?SESSION_ID, Sid, Ctx),
			    case ?SESSION_SERVER_MODULE:get_session(ServerId, Sid) of
				undefined ->
				    ewgi_session:new_session(Ctx1);
				Session ->
				    case ewgi_session:init_session(Ctx1, Session, Timeout, IncludeIp) of
					invalid_session ->
					    %% DISPLAY ERROR!?
					    Ctx_2 = ?MODULE:delete_session(Ctx, StoreArgs),
					    ewgi_session:new_session(Ctx_2);
					Ctx_2 ->
					    Ctx_2
				    end
			    end;
			_ ->
			    %% DISPLAY ERROR! {?MODULE, cookie_tampered}
			    ewgi_session:new_session(Ctx)
		    end
	    end
    end.

store_session(Ctx, [ServerId, CookieName, SecureCookie, _Timeout, IncludeIp]) ->
    Sid = ewgi_api:find_data(?SESSION_ID, Ctx),
    case Sid of
	undefined ->
	    %% New session
	    Session = ewgi_session:get_session(Ctx, IncludeIp),
	    NewId = ?SESSION_SERVER_MODULE:save_new_session(ServerId, Session),
	    SidB64 = cookie_safe_encode(term_to_binary(NewId)),
	    cookie_headers(Ctx, CookieName, SidB64, SecureCookie);
	Sid ->
	    %% existing session
	    Updated = ewgi_session:session_updated(Ctx),
	    if Updated ->
		    Session = ewgi_session:get_session(Ctx, IncludeIp),
		    ?SESSION_SERVER_MODULE:save_session(ServerId, Sid, Session);
	       true -> ok %% nothing to do!
	    end,
	    Ctx
    end.

delete_session(Ctx, [ServerId, CookieName, SecureCookie]) ->
    Sid = ewgi_api:find_data(?SESSION_ID, Ctx),
    case Sid of
	undefined -> ok;
	_ -> ?SESSION_SERVER_MODULE:delete_session(ServerId, Sid)
    end,
    Ctx1 = ewgi_api:store_data(?SESSION_ID, undefined, Ctx),
    cookie_headers(Ctx1, CookieName, [], SecureCookie).

%%====================================================================
%% example functions on how to use the session middleware
%%====================================================================
%% The server reference :: Pid | LocalName | {Node,Name} | {global,Name}
-define(SESSION_SERVER_REF, ewgi_session_server).
-define(SECURE_COOKIE, false).
-define(INCLUDE_IP, true).
-define(SESSION_TIMEOUT, 15 * 60 * 1000). %% 15 minutes
-define(SESSION_STORE_ARGS, [
			     ?SESSION_SERVER_REF,
			     "server_session_id",
			     ?SECURE_COOKIE,
			     ?SESSION_TIMEOUT,
			     ?INCLUDE_IP
			    ]).

create_example(Ctx) ->
    SessionApp = fun ewgi_session:session_create_app/1,
    Ctx1 = ?MODULE:load_session(Ctx, ?SESSION_STORE_ARGS),
    Ctx2 = SessionApp(Ctx1),
    ?MODULE:store_session(Ctx2, ?SESSION_STORE_ARGS).

delete_example(Ctx) ->
    SessionApp = fun ewgi_session:session_delete_app/1,
    Ctx1 = ?MODULE:load_session(Ctx, ?SESSION_STORE_ARGS),
    Ctx2 = SessionApp(Ctx1),
    ?MODULE:store_session(Ctx2, ?SESSION_STORE_ARGS).

