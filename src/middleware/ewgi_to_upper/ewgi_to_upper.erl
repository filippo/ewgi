%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting.

%% @doc to_upper middleware

-module(ewgi_to_upper).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

-export([run/2]).

run(Ctx, []) ->
    Body = ewgi_api:response_message_body(Ctx),
    Body1 = [string:to_upper(erlang:binary_to_list(B)) || B <- Body],
    ewgi_api:response_message_body(Body1, Ctx).
