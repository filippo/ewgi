#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noinput +B

%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Davide Marquês <nesrait@gmail.com>
%%
%% @doc Wrapper script for building a project using an Emakefile.
%% If new modules are detected we do a touch("src/$(APP).app") to trigger (in the
%% Makefile) the recreation of the ebin/$(APP).app file with the new modules.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

main([EBinFolder, AppFileSrc | Others]) ->
	[code:add_patha(Other) || Other <- Others],
	code:add_patha(EBinFolder),
	M1 = filelib:wildcard(EBinFolder ++ "/*.beam"),
	case make:all() of
		up_to_date ->
			%% If there are new/missing files regenerate the .app file
			M2 = filelib:wildcard(EBinFolder ++ "/*.beam"),
			if (M1 =/= M2) ->
				touch(AppFileSrc);
			true -> ok
			end,
			halt(0);
		error ->
			halt(1)
	end;
main(_) ->
	io:format("Invalid arguments to compile.erl!").

-include_lib("kernel/include/file.hrl").

%% Opening/closing file because just calling file:write_file_info/2
%% wasn't getting the job done.
touch(FileName) ->
	{ok, IoDevice} = file:open(FileName, [read, write]),
	{ok, FileInfo} = file:read_file_info(FileName),
	Now = calendar:now_to_local_time(erlang:now()),
	ok = file:write_file_info(FileName, FileInfo#file_info{mtime=Now}),
	file:close(IoDevice).
