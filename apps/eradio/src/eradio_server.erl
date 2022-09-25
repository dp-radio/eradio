-module(eradio_server).
-behaviour(ranch_protocol).

-include_lib("kernel/include/logger.hrl").

%% public API
-export([start/0, stop/0]).

%% ranch callbacks
-export([start_link/3]).

%% private
-export([tcp_connection_init/3]).
-ignore_xref([tcp_connection_init/3]).          % referenced by MFA

%%
%% public API
%%

start() ->
    ?LOG_INFO(?MODULE_STRING " starting on ~p", [node()]),

    ListenIpOpts = case eradio_app:listen_ip() of
                       undefined      -> [];
                       {ok, ListenIp} -> [{ip, ListenIp}]
                   end,
    Port = eradio_app:listen_port(),
    SendBuffer = eradio_app:sndbuf(),
    SocketOpts = ListenIpOpts ++
        [{high_watermark, 0},
         {high_msgq_watermark, 1},
         {port, Port},
         {sndbuf, SendBuffer},
         {send_timeout, 5000}],
    TransportOpts = #{connection_type => supervisor,
                      socket_opts => SocketOpts},

    ApiRoute = {"/v1/[...]", eradio_api_handler, []},
    StreamRoute = {"/stream.mp3", eradio_stream_handler, []},
    WebrootRoutes = case eradio_app:webroot() of
                        undefined    -> [{"/", cowboy_static, {priv_file, eradio_app:application(), "htdocs/index.html"}},
                                         {"/[...]", cowboy_static, {priv_dir, eradio_app:application(), "htdocs"}}];
                       {ok, Webroot} -> [{"/", cowboy_static, {file, filename:join(Webroot, "index.html")}},
                                         {"/[...]", cowboy_static, {dir, Webroot}}]
                   end,
    Routes = [ApiRoute, StreamRoute] ++ WebrootRoutes,
    Dispatch = cowboy_router:compile([{'_', Routes}]),

    ProtocolOpts = #{env => #{dispatch => Dispatch},
                     idle_timeout => infinity,
                     inactivity_timeout => infinity,
                     stream_handlers => [cowboy_stream_h, eradio_server_stream_h]},

    ranch:start_listener(?MODULE, ranch_tcp, TransportOpts, eradio_server, ProtocolOpts).

stop() ->
    ?LOG_INFO(?MODULE_STRING " stopping on ~p", [node()]),
    ranch:stop_listener(?MODULE).

%%
%% ranch callbacks
%%

start_link(Ref, ranch_tcp, ProtocolOpts) ->
    Self = self(),
    Pid = proc_lib:spawn_link(?MODULE, tcp_connection_init, [Self, Ref, ProtocolOpts]),
    {ok, Pid}.

%%
%% private
%%

-spec tcp_connection_init(pid(), ranch:ref(), cowboy:opts()) -> no_return().
tcp_connection_init(Parent, Ref, ProtocolOpts) ->
    {ok, Socket} = ranch:handshake(Ref),
    _ = case maps:get(connection_type, ProtocolOpts, supervisor) of
            worker -> ok;
            supervisor -> process_flag(trap_exit, true)
        end,
    cowboy_http_init(Parent, Ref, Socket, ranch_tcp, ProtocolOpts#{eradio_sock => Socket}).

-dialyzer({no_fail_call, cowboy_http_init/5}).
-spec cowboy_http_init(pid(), ranch:ref(), inet:socket(), module(), cowboy:opts()) -> no_return().
cowboy_http_init(Parent, Ref, Socket, Module, ProtocolOpts) ->
    cowboy_http:init(Parent, Ref, Socket, Module, undefined, ProtocolOpts).
