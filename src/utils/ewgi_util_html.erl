%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak HTML utility methods.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Some code is based on the Python Paste Project which is copyright Ian
%% Bicking, Clark C. Evans, and contributors and released under the MIT
%% license. See: http://pythonpaste.org/

-module(ewgi_util_html).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([escape/1]).

-include("ewgi.hrl").

%% @spec escape(binary() | string()) -> binary() | string()
%% @doc Replace the special characters '&lt;', '&gt;', '&amp;', and '&quot;'
%% to HTML entity sequences.
-spec(escape/1 :: (binary() | string()) -> binary() | string()).
             
escape(Bin) when is_binary(Bin) ->
    list_to_binary(escape(binary_to_list(Bin), []));
escape(S) when is_list(S) ->
    escape(S, []).

-spec(escape/2 :: (string(), list()) -> string()).

escape([], Acc) ->
    lists:reverse(Acc);
escape([$<|Rest], Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape([$>|Rest], Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape([$&|Rest], Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape([$\"|Rest], Acc) ->
    escape(Rest, lists:reverse("&quot;", Acc));
escape([C|Rest], Acc) ->
    escape(Rest, [C|Acc]).
