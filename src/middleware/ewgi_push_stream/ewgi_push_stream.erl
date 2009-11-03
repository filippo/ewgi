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

%% Ewgi Application API
-export([run/2]).

%% Usage examples
-export([chunked_stream_example/1, non_chunked_stream_example/1]).

run(Ctx, [Status, CT, StreamFun]) when is_function(StreamFun, 0) ->
    StreamPid = spawn(StreamFun),
    run(Ctx, [Status, CT, StreamPid]);

run(Ctx, [Status, CT, StreamFun]) when is_function(StreamFun, 1) ->
    StreamPid = spawn(fun() -> StreamFun(Ctx) end),
    run(Ctx, [Status, CT, StreamPid]);

run(Ctx, [Status, ContentType, StreamPid]) when is_pid(StreamPid) ->
    ewgi_api:response_status(Status,
			     ewgi_api:response_headers([{"Content-type", ContentType}], 
						       ewgi_api:response_message_body(StreamPid, Ctx))).

%%--------------------------------------------------------------------
%% Usage Examples
%%--------------------------------------------------------------------
chunked_stream_example(Ctx) ->
    Status = {200, "OK"},
    ContentType = "text/plain",
    StreamFun = stream_function(fun chunked_stream/3),
    ?MODULE:run(Ctx, [Status, ContentType, StreamFun]).

non_chunked_stream_example(Ctx) ->
    Status = {200, "OK"},
    ContentType = "text/plain",
    StreamFun = stream_function(fun non_chunked_stream/3),
    ?MODULE:run(Ctx, [Status, ContentType, StreamFun]).

stream_function(Generator) ->
    fun(Ctx) ->
	    case ewgi_api:stream_process_init() of
		{ok, Connection} ->
		    Generator(Ctx, Connection, 5),
		    ewgi_api:stream_process_end(Connection);
		_ -> ok
	    end
    end.

non_chunked_stream(Ctx, Connection, 0) ->
    IoList = ["done"],
    ewgi_api:stream_process_deliver(Connection, IoList);
non_chunked_stream(Ctx, Connection, Times) ->
    IoList = [data()],
    ewgi_api:stream_process_deliver(Connection, IoList),
    timer:sleep(2000),
    non_chunked_stream(Ctx, Connection, Times - 1).

chunked_stream(Ctx, Connection, 0) ->
    IoList = ["done"],
    ewgi_api:stream_process_deliver_final_chunk(Connection, IoList);
chunked_stream(Ctx, Connection, Times) ->
    IoList = [data()],
    ewgi_api:stream_process_deliver_chunk(Connection, IoList),
    timer:sleep(2000),
    chunked_stream(Ctx, Connection, Times - 1).

data() ->
    httpd_util:rfc1123_date(erlang:localtime()) ++ "\r\n".

