%%%-------------------------------------------------------------------
%% @doc hashfs top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hashfs_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Dir, Algorithm) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Dir, Algorithm]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Dir, Algorithm]) ->
    {ok, { {one_for_all, 0, 1},
	   [#{ id => hashfs
	     , start => {hashfs, start_link, [Dir, Algorithm]} }
	   ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
