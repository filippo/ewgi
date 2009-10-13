#!/usr/bin/env escript
%% -*- erlang -*-

%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Davide Marquês <nesrait@gmail.com>
%%
%% @doc .app file generator: copies AppSrc to AppTarget replacing %VSN% by the
%% Version passed as a parameter and %MODULES% by the names of the modules
%% for which we can find .beam files under the AppTargets directory.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

main([AppSrc, AppTarget, Version]) ->
	EBinFolder = filename:dirname(AppTarget) ++ "/",
	Modules = get_app_modules(EBinFolder),
	generate_app_file(AppSrc, AppTarget, Version, Modules),
	ok;
main(_) ->
	io:format("Invalid arguments to gen_app.erl!").

get_app_modules(EBinFolder) ->
	BeamFiles = filelib:wildcard(EBinFolder ++ "*.beam"),
	Names = [extract_module(beam_lib:info(X)) || X <- BeamFiles],
	string:join(Names, ", ").

extract_module([]) ->
	"undefined";
extract_module([{module, Mod}|_]) ->
	atom_to_list(Mod);
extract_module([_|R]) ->
	extract_module(R).

generate_app_file(AppSrc, AppTarget, Version, Modules) ->
	{ok, AppFile} = file:read_file(AppSrc),
	App1 = re:replace(AppFile, "%VSN%", Version),
	App2 = re:replace(App1, "%MODULES%", Modules),
	ok = file:write_file(AppTarget, App2).

