-module(eradio_source).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("eradio_source.hrl").

%% API
-export([child_spec/0, start/0, stop/0, start_link/0,
         player_state/0, play/0, pause/0, prev/0, next/0,
         veto/1, vetoes/0]).
-export_type([player_state/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).
-define(SUPERVISOR, eradio_sup).

-define(RESPAWN_DELAY_MILLIS, 1000).

-type player_state() :: stopped | {playing, #track{} | unknown}.

-record(state,
        {source_port = undefined :: port(),
         player_state = stopped :: player_state(),
         current_track = undefined :: #track{} | unknown,
         vetoes = #{} :: #{eradio_stream:listener_id() => true}}).

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

player_state() ->
    gen_server:call(?SERVER, player_state).

play() ->
    gen_server:call(?SERVER, {player_command, play}).

pause() ->
    gen_server:call(?SERVER, {player_command, pause}).

prev() ->
    gen_server:call(?SERVER, {player_command, prev}).

next() ->
    gen_server:call(?SERVER, {player_command, next}).

veto(ListenerId) ->
    gen_server:call(?SERVER, {veto, ListenerId}).

vetoes() ->
    gen_server:call(?SERVER, vetoes).

%%
%% gen_server callbacks
%%

init([]) ->
    ?LOG_INFO("source ~1000p started", [self()]),
    State = spawn_source(#state{}),
    {ok, State}.

handle_call(player_state, _From, State) ->
    {reply, {ok, State#state.player_state}, State};

handle_call({player_command, PlayerCommand}, From, State) ->
    try handle_player_command(PlayerCommand, From, State)
    catch _:Error -> {reply, {error, Error}, State} end;

handle_call({veto, ListenerId}, From, State) ->
    try handle_veto(ListenerId, From, State)
    catch _:Error -> {reply, {error, Error}, State} end;

handle_call(vetoes, _From, State) ->
    try count_vetoes(State) of Result -> {reply, {ok, Result}, State}
    catch _:Error -> {reply, {error, Error}, State} end;

handle_call(Request, From, State) ->
    ?LOG_WARNING("unknown call from ~1000p: ~1000p", [From, Request]),
    {reply, unknown_call, State}.

handle_cast(Message, State) ->
    ?LOG_WARNING("unknown cast: ~1000p", [Message]),
    {noreply, State}.

handle_info({SourcePort, {data, Data}}, #state{source_port = SourcePort} = State) ->
    NewState = handle_data(Data, State),
    {noreply, NewState};

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
            case State#state.player_state of
                {playing, _} -> port_command(Port, <<1>>);
                _ -> ok
            end,
            State#state{source_port = Port, player_state = stopped}
    catch
        _:Reason ->
            ?LOG_WARNING("error spawning source ~s: ~1000p", [Exe, Reason]),
            erlang:send_after(?RESPAWN_DELAY_MILLIS, self(), respawn),
            State
    end.

handle_player_command(PlayerCommand, From, #state{source_port = undefined} = State) ->
    ?LOG_INFO("~1000p requesting player ~s but player is dead!", [From, PlayerCommand]),
    {reply, {error, source_dead}, State};

handle_player_command(PlayerCommand, From, State) ->
    ?LOG_INFO("~1000p requesting player ~s", [From, PlayerCommand]),
    Reply = send_player_command(PlayerCommand, State),
    {reply, Reply, State}.

send_player_command(_PlayerCommand, #state{source_port = undefined}) ->
    {error, source_dead};
send_player_command(PlayerCommand, State) ->
    PlayerCommandByte = case PlayerCommand of
                            play  -> 1;
                            pause -> 2;
                            prev  -> 3;
                            next  -> 4;
                            _     -> unknown
                        end,
    case PlayerCommandByte of
        unknown -> {error, bad_command};
        _ ->
            port_command(State#state.source_port, <<PlayerCommandByte>>),
            ok
    end.

handle_veto(_ListenerId, _From, #state{current_track = undefined}=State) ->
    {reply, {error, stopped}, State};
handle_veto(ListenerId, _From, #state{vetoes = Vetoes} = State) when erlang:is_map_key(ListenerId, Vetoes) ->
    NewState = maybe_veto_skip(State),
    {reply, ok, NewState};
handle_veto(ListenerId, _From, State) ->
    case State#state.current_track of
        #track{uri = TrackUri} -> ?LOG_INFO("listener ~b vetoed ~b", [ListenerId, TrackUri]);
        unknown -> ?LOG_INFO("listener ~b vetoed <unknown>")
    end,
    NewState = maybe_veto_skip(State#state{vetoes = (State#state.vetoes)#{ListenerId => true}}),
    {reply, ok, NewState}.

maybe_veto_skip(#state{player_state = stopped}=State) ->
    State;
maybe_veto_skip(State) ->
    {Listeners, Vetoes} = count_vetoes(State),
    case map_size(Vetoes) >= length(Listeners) div 2 of
        true  -> veto_skip(State);
        false -> State
    end.

veto_skip(State) ->
    send_player_command(next, State),
    State.

-spec count_vetoes(#state{}) -> {Listeners :: [eradio_stream:listener_id()], Vetoes :: #{eradio_stream:listener_id() => true}}.
count_vetoes(State) ->
    {ok, Listeners} = eradio_stream_manager:listener_ids(),
    ActiveVetoes = maps:with(Listeners, State#state.vetoes),
    {Listeners, ActiveVetoes}.

handle_data(<<1, LevelByte, Message/binary>>, State) ->
    LevelAtom = case LevelByte of
                    1 -> error;
                    2 -> warning;
                    3 -> info;
                    _ -> debug
                end,
    ?LOG(LevelAtom, "~1000p ~s", [State#state.source_port, Message]),
    State;
handle_data(<<2, Data/binary>>, State) ->
    eradio_stream_manager:send_data(Data),
    State;
handle_data(<<3>>, State) ->
    handle_new_player_state({playing, unknown}, State);
handle_data(<<3, UriLen:16/integer, Uri:UriLen/binary, NameLen:16/integer, Name:NameLen/binary>>, State) ->
    handle_new_player_state({playing, #track{uri = Uri, name = Name}}, State);
handle_data(<<4>>, State) ->
    handle_new_player_state(stopped, State).

handle_new_player_state(PlayerState, #state{player_state = PlayerState}=State) ->
    State;
handle_new_player_state({playing, unknown}, State) ->
    ?LOG_INFO("playback started: <unknown>"),
    State#state{player_state = {playing, unknown}, current_track = unknown, vetoes = #{}};
handle_new_player_state({playing, #track{uri = TrackUri}=Track}, #state{current_track = #track{uri = TrackUri}}=State) ->
    ?LOG_INFO("playback resumed: ~s ~s", [Track#track.uri, Track#track.name]),
    State#state{player_state = {playing, Track}};
handle_new_player_state({playing, #track{}=Track}, State) ->
    ?LOG_INFO("playback started: ~s ~s", [Track#track.uri, Track#track.name]),
    State#state{player_state = {playing, Track}, current_track = Track, vetoes = #{}};
handle_new_player_state(stopped, State) ->
    ?LOG_INFO("playback stopped"),
    State#state{player_state = stopped, vetoes = #{}}.
