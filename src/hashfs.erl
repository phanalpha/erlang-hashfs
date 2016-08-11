%%% ------------------------------------------------------------
%% @doc hashfs
%% @end
%%% ------------------------------------------------------------

-module(hashfs).

-behaviour(gen_server).

%% API
-export([start/0, start_link/2, find/1, save/2, save/1]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% ============================================================
%% API
%% ============================================================

start() ->
    crypto:start(),
    application:start(hashfs).

-spec start_link(file:filename(), crypto:hash_algorithms()) -> _Result.
start_link(Dir, Algorithm) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Dir, Algorithm], []).

-spec find(file:filename()) -> file:filename().
find(Filename) ->
    gen_server:call(?SERVER, {find, Filename}).

-spec save(iodata(), file:filename()) -> file:filename().
save(Buffer, Ext) ->
    gen_server:call(?SERVER, {save, Buffer, Ext}).

-spec save(iodata()) -> file:filename().
save(Buffer) ->
    save(Buffer, "").

%% ============================================================
%% callbacks
%% ============================================================

-record(state, {dir, algorithm}).

init([Dir, Algorithm]) ->
    {ok, #state{dir = Dir, algorithm = Algorithm}}.

handle_call({find, Filename}, _From, #state{dir = Dir} = State) ->
    {reply, find(Filename, Dir), State};
handle_call({save, Buffer, Ext}, _From, #state{dir = Dir, algorithm = Algorithm} = State) ->
    {reply, save(Buffer, Ext, Dir, Algorithm), State};
handle_call(_Request, _From, State) ->
    {reply, unimplemented, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%

-spec find(file:filename(), file:filename()) -> file:filename().
find(Filename, Dir) ->
    filename:absname( filename:join([ Dir
				    , lists:sublist(Filename, 2)
				    , lists:sublist(Filename, 3, 2)
				    , Filename
				    ]) ).

-spec save(iodata(), file:filename(), file:filename(), crypto:hash_algorithms()) -> file:filename().
save(Buffer, Ext, Dir, Algorithm) ->
    Hash = crypto:bytes_to_integer( crypto:hash(Algorithm, Buffer) ),
    Filename = filename:flatten([ io_lib:format("~40.16.0b", [Hash]), Ext ]),
    Path = find(Filename, Dir),
    ok = filelib:ensure_dir(Path),
    {ok, Dev} = file:open(Path, [write, raw]),
    ok = file:write(Dev, Buffer),
    file:close(Dev),

    Filename.
