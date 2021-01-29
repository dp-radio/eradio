-module(eradio_websocket_handler).

-include_lib("kernel/include/logger.hrl").

%% API
-export([stop/1, send/2]).

%% cowboy handler callbacks
-export([init/2]).

%% cowboy websocket callbacks
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

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
%% cowboy handler callbacks
%%

init(Request, {}) ->
    WebsocketOpts = #{max_frame_size => ?FRAME_SIZE_MAX, idle_timeout => ?IDLE_TIMEOUT_MS},
    {cowboy_websocket, Request, {}, WebsocketOpts}.

%%
%% cowboy websocket callbacks
%%

websocket_init({}) ->
    {ok, Pid} = eradio_websocket:start_link(self()),
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
