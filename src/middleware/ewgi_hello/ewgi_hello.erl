%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting.

%% @doc Hello world application and to_upper middleware

-module(ewgi_hello).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

-export([run/2]).

run({ewgi_context, Request, _Response}, []) ->
    ResponseHeaders = [{"Content-type", "text/plain"}],
    Response = {ewgi_response, {200, "OK"}, ResponseHeaders,
                [<<"Hello world!">>], undefined},
    {ewgi_context, Request, Response}.

