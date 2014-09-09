%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Sep 2014 19:21
%%%----------------------------------------------------------------------------
-module(afile_client).

-author("Temple").


%% Erlang Shell Interaction ---------------------------------------------------
%%
%% 1> c(afile_server).
%% {ok,afile_server}
%% 2> c(afile_client).
%% {ok,afile_client}
%% 3> FileServer = afile_server:start("."). <0.43.0>
%% 4> afile_client:get_file(FileServer,"missing"). {error,enoent}
%% 5> afile_client:get_file(FileServer,"afile_server.erl").
%% {ok,<<"-module(afile_server).\n-export([start/1])....}


%%%============================================================================
%%% Public API
%%%============================================================================

-export([
	ls/1,
	get_file/2,
	put_file/2
]).


%%%============================================================================
%%% Public API - Implementation
%%%============================================================================

ls(Server) ->
	Server ! {self(), list_dir},
	receive
		{Server, FileList} ->
			FileList
	end.


get_file(Server, FileName) ->
	Server ! {self(), {get_file, FileName}},
	receive
		{Server, FileData} ->
			io:format("Got file data: ~p~n", [FileData]),
			file:write_file(FileName, FileData, [append])
	end.


put_file(Server, FileName) ->
	FileData = file:read_file(FileName),
	Server ! {self(), {put_file, FileName, FileData}},
	receive
		{Server, ok} ->
			ok
	end.
