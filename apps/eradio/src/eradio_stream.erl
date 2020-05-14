-module(eradio_stream).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1, send_data/2, get_streams/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(PG_GROUP, ?MODULE).

-record(state,
        {stream_pid :: pid() | undefined}).

%%
%% API
%%

start_link(StreamPid) ->
    gen_server:start_link(?MODULE, [StreamPid], []).

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

init([StreamPid]) ->
    ?LOG_INFO("stream ~1000p connected", [self()]),
    process_flag(trap_exit, true),
    try
        ok = pg:join(?PG_GROUP, self())
    catch
        error:undef ->
            ok = pg2:create(?PG_GROUP),
            ok = pg2:join(?PG_GROUP, self())
    end,
    {ok, #state{stream_pid = StreamPid}}.

handle_call(Request, From, State) ->
    ?LOG_WARNING("unknown call from ~1000p: ~1000p", [From, Request]),
    {reply, unknown_call, State}.

handle_cast({send_data, Data}, State) ->
    eradio_stream_handler:send(State#state.stream_pid, Data),
    {noreply, State};

handle_cast(Message, State) ->
    ?LOG_WARNING("unknown cast: ~1000p", [Message]),
    {noreply, State}.

handle_info({'EXIT', StreamPid, _Reason}, #state{stream_pid = StreamPid} = State) ->
    NewState = State#state{stream_pid = undefined},
    {stop, normal, NewState};

handle_info(Message, State) ->
    ?LOG_WARNING("unknown message: ~1000p", [Message]),
    {noreply, State}.

terminate(Reason, State) ->
    ?LOG_INFO("stream ~1000p disconnected: ~1000p", [self(), Reason]),
    case State#state.stream_pid of
        undefined -> ok;
        StreamPid -> eradio_stream_handler:stop(StreamPid)
    end,
    ok.
