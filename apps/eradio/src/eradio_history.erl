-module(eradio_history).

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").
-include("eradio_source.hrl").

%% API
-export([child_spec/0, start_link/0, record_events/2, events_from/1, events_range/2]).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         clean/3,
         dirty/3,
         terminate/3]).

-define(SERVER, ?MODULE).
-define(TABLE, eradio_history).

-define(WRITE_DELAY, 1000).

-record(track_start,
        {track :: #track{}}).

-record(track_stop,
        {pos :: non_neg_integer()}).

-type event() :: #track_start{} | #track_stop{}.

-record(eradio_history,
        {time   :: integer(),
         events :: [event()]}).

-record(state, {}).

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
    gen_statem:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec record_events(Time :: integer(), Events :: [event()]) -> ok.
record_events(Time, Events) ->
    gen_statem:cast({local, ?SERVER}, {record_event, Time, Events}).

-spec events_from(StartTime :: integer()) -> [#eradio_history{}].
events_from(StartTime) ->
    Guards = [{'$1', '>=', StartTime}],
    ets:select(?TABLE, [{#eradio_history{time = '$1', events = '_'}, Guards, ['_']}]).

-spec events_range(StartTime :: integer(), EndTime :: integer()) -> #eradio_history{}.
events_range(StartTime, EndTime) ->
    Guards = [{'$1', '>=', StartTime}, {'$1', '<', EndTime}],
    ets:select(?TABLE, [{#eradio_history{time = '$1', events = '_'}, Guards, ['_']}]).

%%
%% gen_statem callbacks
%%

init({}) ->
    ?LOG_INFO(?MODULE_STRING" started on ~1000p", [node()]),
    load(),
    {ok, clean, #state{}}.

callback_mode() -> state_functions.

clean(cast, {record_events, Time, Events}, State) ->
    insert(Time, Events),
    {next_state, dirty, State, [{state_timeout, ?WRITE_DELAY, write}]};
clean(Type, Event, State) ->
    handle_common(Type, Event, State).

dirty(cast, {record_events, Time, Events}, _State) ->
    insert(Time, Events),
    keep_state_and_data;
dirty(state_timeout, dirty, State) ->
    spawn(fun write/0),
    {next_state, clean, State};
dirty(Type, Event, State) ->
    handle_common(Type, Event, State).

handle_common(Type, Event, _State) ->
    ?LOG_WARNING("unknown ~1000p: ~1000p", [Type, Event]),
    keep_state_and_data.

terminate(Reason, StateName, _State) ->
    ?LOG_INFO(?MODULE_STRING" stopped in state ~s: ~1000p", [StateName, Reason]),
    ok.

%%
%% internal
%%

load() ->
    case ets:file2tab(eradio_app:history_file()) of
        {ok, _Table} -> ok;
        {error, {read_error, {file_error, _, enoent}}} -> ok;
        {error, Reason} ->
            ?LOG_WARNING("error reading history file: ~1000p", [Reason]),
            create()
    end.

create() ->
    ets:new(?TABLE, [set, public, named_table, {keypos, #eradio_history.time}]).

write() ->
    HistoryFile = eradio_app:history_file(),
    NewHistoryFile = HistoryFile ++ ".new",
    case ets:tab2file(?TABLE, NewHistoryFile) of
        ok ->
            case file:rename(NewHistoryFile, HistoryFile) of
                ok -> ok;
                {error, RenameError} ->
                    ?LOG_WARNING("error renaming history file: ~1000p", [RenameError]),
                    {error, RenameError}
            end;
        {error, WriteError} ->
            ?LOG_WARNING("error writing history file: ~1000p", [WriteError]),
            {error, WriteError}
    end.

insert(Time, Events) ->
    NewHistory = case ets:lookup(?TABLE, Time) of
                     [History] ->
                         NewEvents = History#eradio_history.events ++ Events,
                         History#eradio_history{events = NewEvents};
                     [] ->
                         #eradio_history{time = Time, events = Events}
                 end,
    true = ets:insert(?TABLE, NewHistory).
