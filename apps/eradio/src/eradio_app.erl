-module(eradio_app).

-behaviour(application).

%% API
-export([start/0, stop/0, restart/0]).
-export([application/0, history_file/0, listen_ip/0, listen_port/0, sndbuf/0, webroot/0]).
-ignore_xref([application/0]).                  % unused, but useful
-ignore_xref([restart/0, start/0, stop/0]).     % unused, but useful in the shell

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

application() ->
    ?APP.

history_file() ->
    application:get_env(?APP, history_file, "./log/history.tab").

listen_ip() ->
    application:get_env(?APP, listen_ip).

listen_port() ->
    application:get_env(?APP, port, 8080).

sndbuf() ->
    application:get_env(?APP, sndbuf, 8000).

webroot() ->
    application:get_env(?APP, webroot).

%%
%% application callbacks
%%

start(_StartType, _StartArgs) ->
    eradio_server:start(),
    eradio_sup:start_link().

stop(_State) ->
    eradio_server:stop(),
    ok.
