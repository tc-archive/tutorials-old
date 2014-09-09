%%%----------------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Sep 2014 19:21
%%%----------------------------------------------------------------------------
-module(file_client).

-author("Temple").


%% Erlang Shell Interaction ---------------------------------------------------
%%
%% 1> c(file_server).
%% {ok,file_server}
%% 2> c(file_client).
%% {ok,file_client}
%% 3> FileServer = file_server:start("."). <0.43.0>
%% 4> file_client:get_file(FileServer,"missing"). {error,enoent}
%% 5> file_client:get_file(FileServer,"file_server.erl").
%% {ok,<<"-module(afile_server).\n-export([start/1])....}


%%%============================================================================
%%% Public API
%%%============================================================================

-export([
	ls/1,
	get_file/2
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


get_file(Server, File) ->
	Server ! {self(), {get_file, File}},
	receive
		{Server, Content} ->
			Content
	end.
