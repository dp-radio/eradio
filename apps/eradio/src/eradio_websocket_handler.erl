-module(eradio_websocket_handler).
-behaviour(cowboy_sub_protocol).
-behaviour(cowboy_websocket).

-include_lib("kernel/include/logger.hrl").

%% API
-export([stop/1, send/2]).

%% cowboy_sub_protocol callbacks
-export([upgrade/4, upgrade/5]).

%% cowboy websocket callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-define(FRAME_SIZE_MAX, 102400).
-define(IDLE_TIMEOUT_MS, 10000).

-record(state,
        {pid :: pid()}).

%%
%% API
%%

stop(Pid) ->
    Pid ! stop.

send(Pid, Messages) ->
    Pid ! {send, Messages}.

%%
%% cowboy_sub_protocol callbacks
%%

-spec upgrade(Req, Env, module(), any()) ->
    {ok, Req, Env} | {suspend, module(), atom(), [any()]} | {stop, Req}
        when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Request, Env, Module, Arg) ->
    upgrade(Request, Env, Module, Arg, ignored).

-spec upgrade(Req, Env, module(), any(), any()) ->
    {ok, Req, Env} | {suspend, module(), atom(), [any()]} | {stop, Req}
        when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Request, Env, _Module, Arg, _Opts) ->
    WebsocketOpts = #{max_frame_size => ?FRAME_SIZE_MAX, idle_timeout => ?IDLE_TIMEOUT_MS},
    cowboy_websocket:upgrade(Request, Env, ?MODULE, Arg, WebsocketOpts).

%%
%% cowboy websocket callbacks
%%

init(Request, State) ->
    eradio_api_handler:init(Request, State).

websocket_init(Arg) ->
    {ok, Pid} = eradio_websocket:start_link(self(), Arg),
    {ok, #state{pid = Pid}}.

websocket_handle(Frame, State) ->
    {ok, Replies} = eradio_websocket:recv(State#state.pid, Frame),
    {Replies, State}.

websocket_info(stop, State) ->
    {stop, State};

websocket_info({send, Messages}, State) ->
    {Messages, State};

websocket_info(Message, State) ->
    ?LOG_WARNING("unknown message: ~1000p", [Message]),
    {ok, State}.

%%
%% private functions
%%
