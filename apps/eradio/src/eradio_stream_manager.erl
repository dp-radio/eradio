-module(eradio_stream_manager).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([child_spec/0, start_link/0, join/1, send_data/1, listener_ids/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, eradio_stream).

-record(state, {}).

-record(eradio_stream, {id, pid}).

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

join(ListenerId) ->
    gen_server:call(?SERVER, {join, ListenerId, self()}).

send_data(Data) ->
    gen_server:call(?SERVER, {send_data, Data}).

listener_ids() ->
    gen_server:call(?SERVER, listener_ids).

%%
%% gen_server callbacks
%%

init([]) ->
    ?LOG_INFO(?MODULE_STRING " starting on node ~1000p", [node()]),
    ets:new(?TABLE, [set, public, named_table, {keypos, #eradio_stream.id}]),
    {ok, #state{}}.

handle_call({join, ListenerId, Pid}, From, State) ->
    handle_join(ListenerId, Pid, From, State);

handle_call({send_data, Data}, From, State) ->
    handle_send_data(Data, From, State);

handle_call(listener_ids, From, State) ->
    handle_listener_ids(From, State);

handle_call(Request, From, State) ->
    ?LOG_WARNING("unknown call from ~1000p: ~1000p", [From, Request]),
    {reply, unknown_call, State}.

handle_cast(Message, State) ->
    ?LOG_WARNING("unknown cast: ~1000p", [Message]),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, DownPid, Reason}, State) ->
    handle_stream_down(DownPid, Reason, State);

handle_info(Message, State) ->
    ?LOG_WARNING("unknown message: ~1000p", [Message]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG_INFO(?MODULE_STRING " stopping on ~1000p: ~1000p", [node(), Reason]),
    ok.

%%
%% internal
%%

handle_join(ListenerId, Pid, _From, State) ->
    monitor(process, Pid),
    add_stream(#eradio_stream{id = ListenerId, pid = Pid}),
    {reply, ok, State}.

handle_send_data(Data, _From, State) ->
    [eradio_stream:send_data(Stream, Data) || Stream <- stream_pids()],
    {reply, ok, State}.

handle_listener_ids(_From, State) ->
    {reply, {ok, stream_ids()}, State}.

handle_stream_down(Pid, Reason, State) ->
    case ets:match_object(?TABLE, #eradio_stream{id = '_', pid = Pid}) of
        [Stream] ->
            ets:delete(?TABLE, Stream#eradio_stream.id),
            ?LOG_INFO("stream ~p down: ~1000p", [Stream#eradio_stream.id, Reason]);
        [] ->
            ?LOG_INFO("unknown stream ~p down: ~1000p", [Pid, Reason])
    end,
    {noreply, State}.

add_stream(#eradio_stream{id = Id, pid = Pid}=Stream) when is_integer(Id), is_pid(Pid) ->
    ets:insert(?TABLE, Stream).

stream_ids() ->
    ets:select(?TABLE, [{#eradio_stream{id = '$1', pid = '_'}, [], ['$1']}]).

stream_pids() ->
    ets:select(?TABLE, [{#eradio_stream{id = '_', pid = '$1'}, [], ['$1']}]).
