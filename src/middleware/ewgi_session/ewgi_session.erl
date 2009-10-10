%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Session middleware with pluggable storage mechanisms.
%% Based on smak_auth_cookie by Hunter Morris.
%%
%% The loading of session data is done when the middleware is
%% initialized and the storage of session data is only done(!)
%% after calling the middleware downstream.
%%
%% In case of errors in the downstream middleware the updates
%% to session data are not persisted.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ewgi_session).
-author('Hunter Morris <hunter.morris@smarkets.com>').
-author('Davide Marquês <nesrait@gmail.com>').

%% Session API
-export([new_session/1, delete_session/1]).
%% Proplists-looking functions
-export([get_all_values/1, get_value/2, get_value/3, set_value/3, delete/2]).

%% Functions used by the session store modules
-export([init_session/4, session_updated/1, get_session/2]).

%% Util functions used by the store examples
-export([session_create_app/1, session_delete_app/1]).

-include("ewgi.hrl").
-include("session.hrl").

-define(SESSION_STATE, "ewgi.session.session_state").
-define(SESSION_DATA, "ewgi.session.session_data").

%%====================================================================
%% Session API
%%====================================================================
new_session(Ctx) ->
    update_session_data(Ctx, []).

delete_session(Ctx) ->
    update_session_data(Ctx, []).

%%====================================================================
%% Functions that deal with the already loaded session_data
%%====================================================================
get_all_values(Ctx) ->
    ewgi_api:find_data(?SESSION_DATA, Ctx).

get_value(Key, Ctx) ->
	get_value(Key, Ctx, undefined).
	
get_value(Key, Ctx, Default) ->
	case ewgi_api:find_data(?SESSION_DATA, Ctx) of
		undefined ->
			Default;
		Data ->
			proplists:get_value(Key, Data, Default)
	end.

set_value(Key, Value, Ctx) ->
    Data = ewgi_api:find_data(?SESSION_DATA, Ctx),
    Data1 =
	case proplists:is_defined(Key, Data) of
	    true ->
		Rest = proplists:delete(Key, Data),
		[{Key, Value}|Rest];
	    false ->
		[{Key, Value}|Data]
	end,
    update_session_data(Ctx, Data1).

delete(Key, Ctx) ->
    Data = ewgi_api:find_data(?SESSION_DATA, Ctx),
    case proplists:is_defined(Key, Data) of
	true ->
	    Data1 = proplists:delete(Key, Data),
	    update_session_data(Ctx, Data1);
	false ->
	    Ctx
    end.

%%====================================================================
%% Session creation and validation
%%====================================================================
session_updated(Ctx) ->
    updated =:= ewgi_api:find_data(?SESSION_STATE, Ctx).

init_session(Ctx, Session, Timeout, IncludeIp) when is_record(Session, session) ->
    case validate_session(Ctx, Session, Timeout, IncludeIp) of
	{error, _Reason} ->
	    invalid_session;
	ValidSession ->
	    SessionData = ValidSession#session.data,
	    Ctx1 = ewgi_api:store_data(?SESSION_DATA, SessionData, Ctx),
	    ewgi_api:store_data(?SESSION_STATE, loaded, Ctx1)
    end;
init_session(_, _, _, _) ->
    invalid_session.

validate_session(Ctx, Session, Timeout, IncludeIp) ->
    ExpireTime = Session#session.timestamp + Timeout,
    Expired = ewgi_util_calendar:now_utc_ts_ms() >= ExpireTime,
    if Expired ->
	    {error, session_timeout};
       true ->
	    case IncludeIp of
		false ->
		    Session;
		true ->
		    Addr = ewgi_api:remote_addr(Ctx),
		    SavedAddr = Session#session.ip_address,
		    if SavedAddr =:= Addr ->
			    Session;
		       true ->
			    error_logger:error_msg("Saved address: ~p, New address: ~p", [SavedAddr, Addr]),
			    {error, invalid_ip_address}
		    end
	    end
    end.

get_session(Ctx, IncludeIp) ->
    SessionData = ewgi_api:find_data(?SESSION_DATA, Ctx),
    Timestamp = ewgi_util_calendar:now_utc_ts_ms(),
    Session0 = #session{data=SessionData, timestamp=Timestamp},
    if IncludeIp ->
	    Addr = ewgi_api:remote_addr(Ctx),
	    Session0#session{ip_address=Addr};
       true ->
	    Session0
    end.

%%====================================================================
%% example functions on how to use the session middleware
%%====================================================================
session_create_app(Ctx) ->
    case ?MODULE:get_value(name, Ctx) of
	undefined ->
	    Name = "Bill",
	    Body = ["Hello stranger! I'll call you ", Name, " from now on! (please refresh the page)"],
	    Ctx1 = ?MODULE:set_value(name, "Bill", Ctx);
	Name ->
	    Body = ["Hello ", Name, "! Nice to see you again! "
		    "(no point in reloading again, delete the session by "
		    "adding /delete to the current url)"],
	    Ctx1 = Ctx
    end,
    Headers = [{"Content-type", "text/plain"}] ++ ewgi_api:response_headers(Ctx1),
    Ctx2 = ewgi_api:response_headers(Headers, Ctx1),
    Ctx3 = ewgi_api:response_status({200, "OK"}, Ctx2),
    ewgi_api:response_message_body(Body, Ctx3).

session_delete_app(Ctx) ->
    Ctx1 = ?MODULE:delete_session(Ctx),
    ewgi_api:response_message_body("Deleted session!",
				   ewgi_api:response_status({200, "OK"}, Ctx1)).

%%====================================================================
%% Internal functions
%%====================================================================
update_session_data(Ctx, SessionData) ->
    Ctx1 = ewgi_api:store_data(?SESSION_DATA, SessionData, Ctx),
    ewgi_api:store_data(?SESSION_STATE, updated, Ctx1).

