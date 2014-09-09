%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Sep 2014 19:11
%%%----------------------------------------------------------------------------
-module(file_server).
-author("Temple").


%% Erlang Shell Interaction ---------------------------------------------------
%%
%% 1> c(file_server).
%% {ok,file_server}
%% 2> FileServer = file_server:start("."). <0.47.0>
%% 3> FileServer ! {self(), list_dir}. {<0.31.0>,list_dir}
%% 4> receive X -> X end.
%% { < 0.47.0>,
%% {ok, ["file1.nfo", "file2.erl","file3.erl"]}}


%%%============================================================================
%%% Public API
%%%============================================================================

-export([
	start/1,
	loop/1
]).


%%%============================================================================
%%% Public API - Implementation
%%%============================================================================

start(Dir) -> spawn(file_server, loop, [Dir]).


loop(Dir) ->
	receive
		{Client, list_dir} ->
			Client ! {self(), file:list_dir(Dir)};
		{Client, {get_file, File}} ->
			Full = filename:join(Dir, File),
			Client ! {self(), file:read_file(Full)}
	end,
	loop(Dir).
