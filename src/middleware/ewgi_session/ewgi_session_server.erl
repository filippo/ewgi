%% @author Dave Bryson [http://weblog.miceda.org]
%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright Dave Bryson 2008-2009
%%
%% @doc Server-side session store.
%% Adapted from beepbeep_session_server by Dave Bryson.
%% All data is stored on the server.
%% Only a unique session id is exchanged with the client.
%% Inspired by the Yaws Session Server.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ewgi_session_server).
-author('Dave Bryson <http://weblog.miceda.org>').
-author("Davide Marquês <nesrait@gmail.com>").

-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_session/2, save_new_session/2, save_session/3, delete_session/2]).
-export([purge_stale_sessions/1, purge_stale_sessions/2]).

-include("session.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts and links the server
%%--------------------------------------------------------------------
start_link() ->
    start_link({local, ?MODULE}).

start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

stop(Server) ->
    gen_server:cast(Server, stop).

get_session(Server, Sid) ->
    gen_server:call(Server, {get_session, Sid}).

save_new_session(Server, Data) ->
    gen_server:call(Server, {save_new_session, Data}).
	
save_session(Server, Sid, Data) ->
    gen_server:call(Server, {save_session, Sid, Data}).

delete_session(Server, Sid) ->
    gen_server:call(Server, {delete_session, Sid}).

purge_stale_sessions(Timeout) ->
    purge_stale_sessions(?MODULE, Timeout).

purge_stale_sessions(Server, Timeout) ->
    gen_server:call(Server, {purge_state_sessions, Timeout}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    ets:new(?MODULE, [set, named_table, {keypos, 1}]),
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    {ok, undefined}.

handle_call({get_session, Sid}, _From, _State) ->
    Data = case ets:lookup(?MODULE, Sid) of
	       [{_,Session}] ->
		   Session;
	       [] ->
		   undefined
	   end,
    {reply, Data, undefined};

handle_call({save_new_session, Session}, _From, _State) ->
	NewSid = make_session_id(),	
    ets:insert(?MODULE, {NewSid,Session}),
    {reply, NewSid, undefined};

handle_call({save_session, Sid, Session}, _From, _State) ->
    ets:insert(?MODULE, {Sid,Session}),
    {reply, ok, undefined};


handle_call({delete_session, Sid}, _From, _State) ->
    ets:delete(?MODULE, Sid),
    {reply, ok, undefined};

handle_call({purge_state_sessions, Timeout}, _From, _State) ->
    MinCreationDate = ewgi_util_calendar:now_utc_ts_ms() - Timeout,
    Pattern =
	ets:fun2ms(fun({_,#session{timestamp=Timestamp}}=A) -> Timestamp =< MinCreationDate end),
    NumDeleted = ets:select_delete(?MODULE, Pattern),
    {reply, {num_deleted, NumDeleted}, undefined}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

make_session_id() ->
    Data = crypto:rand_bytes(2048),
    Sha_list = binary_to_list(crypto:sha(Data)),
    Id = lists:flatten(list_to_hex(Sha_list)),
    Id.

%% Convert Integer from the SHA to Hex
list_to_hex(L)->
	[int_to_hex(X) || X <- L].
 
int_to_hex(N) when N < 256 -> 
	[hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).
