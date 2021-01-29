-module(eradio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%
%% API
%%

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%
%% supervisor callbacks
%%

init([]) ->
    Mods = [eradio_stream_manager, eradio_source, eradio_websocket_manager],
    ChildSpecs = [Mod:child_spec() || Mod <- Mods],
    SupervisorFlags =
        #{strategy => one_for_all,
          intensity => 5,
          period => 1},
    {ok, {SupervisorFlags, ChildSpecs}}.
