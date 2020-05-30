-module(eradio_stream).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/2, send_data/2, get_streams/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(PG_GROUP, ?MODULE).

-record(state,
        {stream_pid  :: pid() | undefined,
         id          :: integer(),
         sent = 0    :: integer(),
         acked = 0   :: integer(),
         dropped = 0 :: integer()}).

%%
%% API
%%

start_link(StreamPid, ListenerId) when is_integer(ListenerId) ->
    gen_server:start_link(?MODULE, [StreamPid, ListenerId], []).

send_data(Pid, Data) ->
    gen_server:cast(Pid, {send_data, Data}).

get_streams() ->
    try
        pg:get_members(?PG_GROUP)
    catch
        error:undef ->
            case pg2:get_members(?PG_GROUP) of
                Pids when is_list(Pids) -> Pids;
                {error, {no_such_group, _}} -> []
            end
    end.

%%
%% gen_server callbacks
%%

init([StreamPid, ListenerId]) ->
    ?LOG_INFO("stream ~b connected", [ListenerId]),
    process_flag(trap_exit, true),
    try
        ok = pg:join(?PG_GROUP, self())
    catch
        error:undef ->
            ok = pg2:create(?PG_GROUP),
            ok = pg2:join(?PG_GROUP, self())
    end,
    {ok, #state{stream_pid = StreamPid, id = ListenerId}}.

handle_call(Request, From, State) ->
    ?LOG_WARNING("unknown call from ~1000p: ~1000p", [From, Request]),
    {reply, unknown_call, State}.

handle_cast({send_data, Data}, State) ->
    NewState = handle_send_data(Data, State),
    {noreply, NewState};

handle_cast(Message, State) ->
    ?LOG_WARNING("unknown cast: ~1000p", [Message]),
    {noreply, State}.

handle_info({data_ack, DataAcked}, State) ->
    NewState = handle_data_ack(DataAcked, State),
    {noreply, NewState};

handle_info({'EXIT', StreamPid, _Reason}, #state{stream_pid = StreamPid} = State) ->
    NewState = State#state{stream_pid = undefined},
    {stop, normal, NewState};

handle_info(Message, State) ->
    ?LOG_WARNING("unknown message: ~1000p", [Message]),
    {noreply, State}.

terminate(Reason, State) ->
    ?LOG_INFO("stream ~b disconnected: ~1000p", [State#state.id, Reason]),
    case State#state.stream_pid of
        undefined -> ok;
        StreamPid -> eradio_stream_handler:stop(StreamPid)
    end,
    ok.

handle_send_data(Data, #state{sent = Sent, acked = Acked}=State) when Sent =< Acked ->
    case State#state.dropped of
        0 -> ok;
        Dropped -> ?LOG_INFO("stream ~b dropped ~b packets", [State#state.id, Dropped])
    end,
    NewSent = Sent + 1,
    eradio_stream_handler:send(State#state.stream_pid, {self(), NewSent}, Data),
    State#state{sent = NewSent, dropped = 0};
handle_send_data(_Data, State) ->
    State#state{dropped = State#state.dropped + 1}.

handle_data_ack(DataAcked, #state{acked = Acked}=State) when DataAcked >= Acked ->
    State#state{acked = DataAcked};
handle_data_ack(_DataAcked, State) ->
    State.
