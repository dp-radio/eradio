-module(test_stream_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%% common_test callbacks
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

%% tests
-export([test_stream/1]).

-define(TIMEOUT, 1000).
-define(BASE_URL, "http://localhost").
-define(HTTPC_PROFILE, ?MODULE).

%%
%% common_test callbacks
%%

suite() -> [{timetrap, ?TIMEOUT}].

all() -> [test_stream].

init_per_suite(_Config) ->
    {ok, _ApplicationPid} = eradio_app:start(),
    {ok, {local, ListenPath}} = eradio_app:listen_ip(),

    {ok, _HttpcProfilePid} = inets:start(httpc, [{profile, ?HTTPC_PROFILE}]),
    ok = httpc:set_options([{ipfamily, local}, {unix_socket, ListenPath}], ?HTTPC_PROFILE),
    [].

end_per_suite(_Config) ->
    ok = inets:stop(httpc, ?HTTPC_PROFILE),
    ok = eradio_app:stop(),
    ok.

%%
%% tests
%%

test_stream(_Config) ->
    Opts = [{sync, false}, {stream, self}],
    {ok, RequestId} = request(get, {"/stream.mp3?listener_id=1", []}, [], Opts),
    receive
        {http, {RequestId, stream_start, _Headers}} -> ok;
        {http, {RequestId, Result}} -> ct:fail(Result)
    end.

%%
%% private
%%

request(Method, {Path, Headers}, HTTPOpts, Opts) ->
    httpc:request(Method, {?BASE_URL ++ Path, Headers}, [{timeout, ?TIMEOUT} | HTTPOpts], Opts, ?HTTPC_PROFILE).
