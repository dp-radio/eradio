-module(eradio_app).

-behaviour(application).

%% API
-export([start/0, stop/0, restart/0]).

%% application callbacks
-export([start/2, stop/1]).

-define(APP, eradio).

%%
%% API
%%

start() ->
    application:ensure_all_started(?APP).

stop() ->
    application:stop(?APP).

restart() ->
    stop(),
    start().

%%
%% application callbacks
%%

start(_StartType, _StartArgs) ->
    eradio_server:start(),
    eradio_sup:start_link().

stop(_State) ->
    eradio_server:stop(),
    ok.
