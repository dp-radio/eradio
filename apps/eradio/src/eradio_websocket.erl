-module(eradio_websocket).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_notify_opts/0, start_link/2, send_notify/1, recv/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(state,
        {websocket_pid :: pid() | undefined}).

%%
%% API
%%

-spec start_notify_opts() -> any().
start_notify_opts() ->
    notify_websocket.

start_link(WebsocketPid, Arg) ->
    gen_server:start_link(?MODULE, {WebsocketPid, Arg}, []).

send_notify(Pid) ->
    gen_server:cast(Pid, send_notify).

recv(Pid, Message) ->
    gen_server:call(Pid, {recv, Message}).

%%
%% gen_server callbacks
%%

init({WebsocketPid, Arg}) ->
    ?LOG_INFO("websocket ~1000p connected", [WebsocketPid]),
    process_flag(trap_exit, true),
    case Arg of
        notify_websocket ->
            ok = eradio_websocket_manager:join()
    end,
    {ok, #state{websocket_pid = WebsocketPid}}.

handle_call({recv, Message}, _From, State) ->
    handle_recv(Message, State);

handle_call(Request, From, State) ->
    ?LOG_WARNING("unknown call from ~1000p: ~1000p", [From, Request]),
    {reply, unknown_call, State}.

handle_cast(send_notify, State) ->
    websocket_send([{text, jsone:encode(#{action => notify})}], State),
    {noreply, State};
handle_cast(Message, State) ->
    ?LOG_WARNING("unknown cast: ~1000p", [Message]),
    {noreply, State}.

handle_info({'EXIT', WebsocketPid, _Reason}, #state{websocket_pid = WebsocketPid} = State) ->
    NewState = State#state{websocket_pid = undefined},
    {stop, normal, NewState};

handle_info(Message, State) ->
    ?LOG_WARNING("unknown message: ~1000p", [Message]),
    {noreply, State}.

terminate(Reason, State) ->
    ?LOG_INFO("websocket ~1000p disconnected: ~1000p", [State#state.websocket_pid, Reason]),
    case State#state.websocket_pid of
        undefined -> ok;
        WebsocketPid -> eradio_websocket_handler:stop(WebsocketPid)
    end,
    ok.

%%
%% private functions
%%

websocket_send(Messages, State) ->
    eradio_websocket_handler:send(State#state.websocket_pid, Messages).

handle_recv(ping, State) ->
    {reply, {ok, [pong]}, State};
handle_recv({ping, _}, State) ->
    {reply, {ok, [pong]}, State};
handle_recv(pong, State) ->
    {reply, {ok, []}, State};
handle_recv({pong, _}, State) ->
    {reply, {ok, []}, State};
handle_recv(close, State) ->
    ?LOG_INFO("websocket ~s closed", []),
    {stop, normal, State};
handle_recv({close, Code, Reason}, State) ->
    ?LOG_INFO("websocket ~s closed for reason ~b: ~s", [Code, Reason]),
    {stop, normal, State};
handle_recv({_, <<>>}, State) ->
    {reply, {ok, [{binary, <<>>}]}, State};
handle_recv(Message, State) ->
    ?LOG_WARNING("unknown websocket message: ~1000p", [Message]),
    {reply, {ok, []}, State}.
