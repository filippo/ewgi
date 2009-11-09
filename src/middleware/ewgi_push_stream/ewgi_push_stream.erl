%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Davide Marquês <nesrait@gmail.com>
%%
%% @doc ewgi Push streams middleware.
%% The difference for regular [pull-]streams is that those are synchronously polled
%% by the ewgi server for more data. With pull streams the ewgi server is notified
%% when there is more data available.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
-module(ewgi_push_stream).
-author('Davide Marquês <nesrait@gmail.com>').

-define(STREAM_INIT_TIMEOUT, 5000).

-define(EX_STREAM_DATA, ["ini\r\n", "mini\r\n", "mini\r\n", "mo\r\n"]).

%% Ewgi Application API
-export([run/2]).

%% Usage examples
-export([
		chunked_stream_example/1,
		non_chunked_stream_example/1,
		ewgi_free_stream_example/1
	]).

%% The body field is used to specify which process will be producing data.
%% The various headers and data are all send asynchronously to the
%% ewgi gateway when available.
%% The given timeout defines how long to want before issuing a 504 response.
run(Ctx, [StreamPid]) when is_pid(StreamPid) ->
	PS = {push_stream, StreamPid, ?STREAM_INIT_TIMEOUT},
	ewgi_api:response_message_body(PS, Ctx);
	
run(Ctx, [StreamPid, Timeout]) when is_pid(StreamPid) ->
	PS = {push_stream, StreamPid, Timeout},
	ewgi_api:response_message_body(PS, Ctx).

%%--------------------------------------------------------------------
%% Chunked stream example
%%--------------------------------------------------------------------
%% N.B.: there's no point in setting headers in the ewgi_context
%% passed to ewgi_push_stream:run/2. Those headers will be ignore!
chunked_stream_example(Ctx) ->
	StreamPid = spawn(fun() -> chunked_stream(Ctx) end),
    ?MODULE:run(Ctx, [StreamPid]).

%% The ewgi_context that is passed to ewgi_api:stream_process_init/2 is 
%% the one from where the status code and headers are read from.
%% The default is a chunked response (see below for non-chunked responses).
chunked_stream(Ctx0) ->
	Status = {200, "OK"},
	H = ewgi_api:response_headers(Ctx0),
	CTHeader = {"Content-type", "text/plain"},
	Ctx = ewgi_api:response_headers([CTHeader|H],
					ewgi_api:response_status(Status, Ctx0)),
	
	CSEnd = "chunked_stream_end",
	case ewgi_api:stream_process_init(Ctx, chunked) of
	{ok, Connection} ->
		lists:foreach(fun(Word) ->
			ewgi_api:stream_process_deliver_chunk(Connection, Word),
			timer:sleep(1000)
			end, ?EX_STREAM_DATA),
		ewgi_api:stream_process_deliver_final_chunk(Connection, CSEnd),
		ewgi_api:stream_process_end(Connection);
	_ -> ok
	end.
	
%%--------------------------------------------------------------------
%% Non-Chunked stream example
%%--------------------------------------------------------------------
%% N.B.: there's no point in setting headers in the ewgi_context
%% passed to ewgi_push_stream:run/2. Those headers will be ignore!
non_chunked_stream_example(Ctx) ->
	StreamPid = spawn(fun() -> non_chunked_stream(Ctx) end),
    ?MODULE:run(Ctx, [StreamPid]).

%% The ewgi_context that is passed to ewgi_api:stream_process_init/2 is 
%% the one from where the status code and headers are read from.
%% By passing a content-length we specify that this woun't be a chunked response.
non_chunked_stream(Ctx0) ->
	Status = {200, "OK"},
	H = ewgi_api:response_headers(Ctx0),
	CTHeader = {"Content-type", "text/plain"},
	Ctx = ewgi_api:response_headers([CTHeader|H],
					ewgi_api:response_status(Status, Ctx0)),

	NCSEnd = "non_chunked_stream_end",
	ContentLength = iolist_size(?EX_STREAM_DATA) + iolist_size(NCSEnd),
	case ewgi_api:stream_process_init(Ctx, ContentLength) of
	{ok, Connection} ->
		lists:foreach(fun(Word) ->
			ewgi_api:stream_process_deliver(Connection, Word),
			timer:sleep(1000)
			end, ?EX_STREAM_DATA),
		ewgi_api:stream_process_deliver(Connection, NCSEnd),
		ewgi_api:stream_process_end(Connection);
	_ -> ok
	end.

%%--------------------------------------------------------------------
%% Ewgi-free stream
%% Just showing off that any process can serve as a push stream
%%--------------------------------------------------------------------
ewgi_free_stream_example(Ctx) ->
	StreamPid = spawn(fun ewgi_free_stream/0),
    ?MODULE:run(Ctx, [StreamPid]).
	
ewgi_free_stream() ->
	EFSEnd = "ewgi_free_stream_end",
	StatusCode = 200,
	Headers = [{"Content-type", "text/plain"}],
	case ewgi_api:stream_process_init(StatusCode, Headers, chunked) of
	{ok, Connection} ->
		lists:foreach(fun(Word) ->
			ewgi_api:stream_process_deliver_chunk(Connection, Word),
			timer:sleep(1000)
			end, ?EX_STREAM_DATA),
		ewgi_api:stream_process_deliver_final_chunk(Connection, EFSEnd),
		ewgi_api:stream_process_end(Connection);
	_ -> ok
	end.
