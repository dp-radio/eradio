-module(eradio_server).

-include_lib("kernel/include/logger.hrl").

-export([start/0, stop/0]).

start() ->
    ?LOG_INFO(?MODULE_STRING " starting on ~p", [node()]),
    {ok, App} = application:get_application(),

    ListenIpOpts = case application:get_env(App, listen_ip) of
                       undefined      -> [];
                       {ok, ListenIp} -> [{listen_ip, ListenIp}]
                   end,
    Port = application:get_env(App, port, 8080),
    SendBuffer = application:get_env(App, sndbuf, 8000),
    TransportOpts = ListenIpOpts ++ [{port, Port},
                                     {sndbuf, SendBuffer},
                                     {send_timeout, 5000},
                                     {high_watermark, 0},
                                     {high_msgq_watermark, 1}],

    ApiRoute = {"/v1/[...]", eradio_api_handler, []},
    StreamRoute = {"/stream.mp3", eradio_stream_handler, []},
    WebrootRoute = case application:get_env(App, webroot) of
                       undefined     -> {"/[index.html]", cowboy_static, {priv_file, App, "default_index.html"}};
                       {ok, Webroot} -> {"/[...]", cowboy_static, {dir, Webroot}}
                   end,
    Routes = [ApiRoute, StreamRoute, WebrootRoute],
    Dispatch = cowboy_router:compile([{'_', Routes}]),

    ProtocolOpts = #{env => #{dispatch => Dispatch}},

    cowboy:start_clear(?MODULE, TransportOpts, ProtocolOpts).

stop() ->
    ?LOG_INFO(?MODULE_STRING " stopping on ~p", [node()]),
    cowboy:stop_listener(?MODULE).
