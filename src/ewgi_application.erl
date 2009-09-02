%%%-------------------------------------------------------------------
%%% File    : ewgi_application.erl
%%% Authors : Filippo Pacini <filippo.pacini@gmail.com>
%%%           Hunter Morris <huntermorris@gmail.com>
%%% License :
%%% The contents of this file are subject to the Mozilla Public
%%% License Version 1.1 (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of
%%% the License at http://www.mozilla.org/MPL/
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and
%%% limitations under the License.
%%% The Initial Developer of the Original Code is S.G. Consulting
%%% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
%%% 2007 S.G. Consulting srl. All Rights Reserved.
%%%
%%% @doc 
%%% <p>ewgi application behaviour.</p>
%%%
%%% @end
%%%
%%% Created : 22 May 2008 by Hunter Morris <huntermorris@gmail.com>
%%%-------------------------------------------------------------------
-module(ewgi_application).

%% Public API
-export([run/2]).

%% Useful middleware
-export([module_mw/2, rpc_mw/4, mfa_mw/3]).

-include_lib("ewgi.hrl").

%% @spec run(Application::ewgi_app(), Context::ewgi_context()) -> Context1::ewgi_context()
%% @doc Runs an EWGI application with Context and returns the new Context1.
-spec run(ewgi_app(), ewgi_context()) -> ewgi_context().
run(Application, Context) when is_function(Application, 1) ->
    Application(Context).

%% @spec module_mw(Module::term(), Args:any()) -> ewgi_app()
%% @doc Produces a middleware application which calls the run/1 function exported by Module.
-spec module_mw(any(), any()) -> ewgi_app().
module_mw(Module, Args) ->
    F = fun(Context) ->
                Module:run(Context, Args)
        end,
    F.

%% @spec rpc_mw(Node::atom(), Module::atom(), Fun::atom(), Args:any()) -> ewgi_app()
%% @doc Produces a middleware application which calls the remote function Module:Fun on Node.
-spec rpc_mw(atom(), atom(), atom(), any()) -> ewgi_app().
rpc_mw(Node, Module, Fun, Args) ->
    F = fun(Context) ->
                rpc:call(Node, Module, Fun, [Context, Args])
        end,
    F.

%% @spec mfa_mw(Module::atom(), Fun::atom(), Args:any()) -> ewgi_app()
%% @doc Produces a middleware application which calls the function Module:Fun(Context, Args)
-spec mfa_mw(atom(), atom(), any()) -> ewgi_app().
mfa_mw(Module, Fun, Args) when is_atom(Module), is_atom(Fun) ->
    F = fun(Context) ->
                apply(Module, Fun, [Context, Args])
        end,
    F.
