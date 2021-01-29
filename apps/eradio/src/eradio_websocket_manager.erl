-module(eradio_websocket_manager).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([child_spec/0, start_link/0, join/0, send_notify/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, eradio_websocket).

-record(state, {}).

-record(eradio_websocket, {pid}).

%%
%% API
%%

child_spec() ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, []},
      restart => transient,
      shutdown => infinity,
      type => worker,
      modules => [?MODULE]}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join() ->
    gen_server:call(?SERVER, {join, self()}).

send_notify() ->
    gen_server:call(?SERVER, send_notify).

%%
%% gen_server callbacks
%%

init([]) ->
    ?LOG_INFO(?MODULE_STRING " starting on node ~1000p", [node()]),
    ets:new(?TABLE, [set, public, named_table, {keypos, #eradio_websocket.pid}]),
    {ok, #state{}}.

handle_call({join, Pid}, From, State) ->
    handle_join(Pid, From, State);

handle_call(send_notify, From, State) ->
    handle_send_notify(From, State);

handle_call(Request, From, State) ->
    ?LOG_WARNING("unknown call from ~1000p: ~1000p", [From, Request]),
    {reply, unknown_call, State}.

handle_cast(Message, State) ->
    ?LOG_WARNING("unknown cast: ~1000p", [Message]),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, DownPid, Reason}, State) ->
    handle_websocket_down(DownPid, Reason, State);

handle_info(Message, State) ->
    ?LOG_WARNING("unknown message: ~1000p", [Message]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG_INFO(?MODULE_STRING " stopping on ~1000p: ~1000p", [node(), Reason]),
    ok.

%%
%% internal
%%

handle_join(Pid, _From, State) ->
    monitor(process, Pid),
    add_websocket(#eradio_websocket{pid = Pid}),
    {reply, ok, State}.

handle_send_notify(_From, State) ->
    [eradio_websocket:send_notify(Websocket) || Websocket <- websocket_pids()],
    {reply, ok, State}.

handle_websocket_down(Pid, Reason, State) ->
    ets:delete(?TABLE, Pid),
    ?LOG_INFO("websocket ~p down: ~1000p", [Pid, Reason]),
    {noreply, State}.

add_websocket(#eradio_websocket{pid = Pid}=Websocket) when is_pid(Pid) ->
    ets:insert(?TABLE, Websocket).

websocket_pids() ->
    ets:select(?TABLE, [{#eradio_websocket{pid = '$1'}, [], ['$1']}]).
