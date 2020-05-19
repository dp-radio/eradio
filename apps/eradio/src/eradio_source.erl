-module(eradio_source).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([child_spec/0, start/0, stop/0, start_link/0, next/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).
-define(SUPERVISOR, eradio_sup).

-define(RESPAWN_DELAY_MILLIS, 1000).

-record(state,
        {source_port = undefined :: port()}).

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

start() ->
    supervisor:start_child(?SUPERVISOR, child_spec()).

stop() ->
    supervisor:terminate_child(?SUPERVISOR, ?MODULE),
    supervisor:delete_child(?SUPERVISOR, ?SERVER).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

next() ->
    gen_server:call(?SERVER, next).

%%
%% gen_server callbacks
%%

init([]) ->
    ?LOG_INFO("source ~1000p started", [self()]),
    State = spawn_source(#state{}),
    {ok, State}.

handle_call(next, From, State) ->
    ?LOG_INFO("~1000p requesting next song", [From]),
    port_command(State#state.source_port, <<1>>),
    {reply, ok, State};

handle_call(Request, From, State) ->
    ?LOG_WARNING("unknown call from ~1000p: ~1000p", [From, Request]),
    {reply, unknown_call, State}.

handle_cast(Message, State) ->
    ?LOG_WARNING("unknown cast: ~1000p", [Message]),
    {noreply, State}.

handle_info({SourcePort, {data, Data}}, #state{source_port = SourcePort} = State) ->
    handle_data(Data, State),
    {noreply, State};

handle_info(respawn, #state{source_port = undefined} = State) ->
    NewState = spawn_source(State),
    {noreply, NewState};
handle_info(respawn, State) ->
    {noreply, State};

handle_info({SourcePort, {exit_status, ExitCode}}, #state{source_port = SourcePort} = State) ->
    ?LOG_WARNING("source ~1000p died: ~1000p", [SourcePort, ExitCode]),
    erlang:send_after(?RESPAWN_DELAY_MILLIS, self(), respawn),
    NewState = State#state{source_port = undefined},
    {noreply, NewState};

handle_info(Message, State) ->
    ?LOG_WARNING("unknown message: ~1000p", [Message]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG_INFO("source ~1000p stopped: ~1000p", [self(), Reason]),
    ok.

%%
%% internal
%%

source_exe() ->
    case application:get_env(source_exe) of
        {ok, {Exe, [_|_]=Args}} -> {Exe, Args};
        undefined ->
            {ok, Application} = application:get_application(),
            PrivDir = code:priv_dir(Application),
            DefaultExe = filename:join([PrivDir, "source"]),
            DefaultArgs = [],
            {DefaultExe, DefaultArgs}
    end.

spawn_source(State) ->
    {Exe, Args} = source_exe(),
    PortSettings =
        [{packet, 4},
         {args, Args},
         exit_status,
         binary],
    try open_port({spawn_executable, Exe}, PortSettings) of
        Port ->
            ?LOG_INFO("spawned source ~s: ~1000p", [Exe, Port]),
            State#state{source_port = Port}
    catch
        _:Reason ->
            ?LOG_WARNING("error spawning source ~s: ~1000p", [Exe, Reason]),
            erlang:send_after(?RESPAWN_DELAY_MILLIS, self(), respawn),
            State
    end.

handle_data(<<1, LevelByte, Message/binary>>, State) ->
    LevelAtom = case LevelByte of
                    1 -> error;
                    2 -> warning;
                    3 -> info;
                    _ -> debug
                end,
    ?LOG(LevelAtom, "~1000p ~s", [State#state.source_port, Message]);
handle_data(<<2, Data/binary>>, _State) ->
    [eradio_stream:send_data(Stream, Data) || Stream <- eradio_stream:get_streams()];
handle_data(<<3>>, _State) ->
    ?LOG_INFO("playback started: <unknown>");
handle_data(<<3, Id:128/integer, Name/binary>>, _State) ->
    ?LOG_INFO("playback started: ~b ~s", [Id, Name]);
handle_data(<<4>>, _State) ->
    ?LOG_INFO("playback stopped").
