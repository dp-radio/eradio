-module(eradio_source).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([child_spec/0, start/0, stop/0, start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).
-define(SUPERVISOR, eradio_sup).

-record(state,
        {}).

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

%%
%% gen_server callbacks
%%

init([]) ->
    ?LOG_INFO("source ~1000p started", [self()]),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?LOG_WARNING("unknown call from ~1000p: ~1000p", [From, Request]),
    {reply, unknown_call, State}.

handle_cast(Message, State) ->
    ?LOG_WARNING("unknown cast: ~1000p", [Message]),
    {noreply, State}.

handle_info(Message, State) ->
    ?LOG_WARNING("unknown message: ~1000p", [Message]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG_INFO("source ~1000p stopped: ~1000p", [self(), Reason]),
    ok.
