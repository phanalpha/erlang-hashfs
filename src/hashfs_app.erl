%%%-------------------------------------------------------------------
%% @doc hashfs public API
%% @end
%%%-------------------------------------------------------------------

-module(hashfs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Dir} = application:get_env(dir),
    Algorithm = list_to_atom( application:get_env(hashfs, algorithm, "sha") ),

    hashfs_sup:start_link(Dir, Algorithm).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
