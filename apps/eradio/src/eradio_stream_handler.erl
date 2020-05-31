-module(eradio_stream_handler).

-include_lib("kernel/include/logger.hrl").

%% API
-export([send/3, stop/1]).

%% cowboy handler callbacks
-export([init/2, info/3]).

%%
%% API
%%

send(Pid, {FromPid, Tag}, Data) ->
     Pid ! {data, {FromPid, Tag}, Data}.

stop(Pid) ->
    Pid ! eof.

%%
%% cowboy handler callbacks
%%

init(Request, State) ->
    try cowboy_req:match_qs([{listener_id, int}], Request) of
        #{listener_id := ListenerId} ->
            start_streaming(Request, ListenerId, State)
    catch
        error:Reason ->
            ?LOG_INFO("invalid stream request: ~1000p", [Reason]),
            NotFoundRequest = cowboy_req:reply(400, #{}, <<>>, Request),
            {ok, NotFoundRequest, State}
    end.
info({data, {FromPid, Tag}, Data}, Request, State) ->
    ok = cowboy_req:stream_body(Data, nofin, Request),
    FromPid ! {data_ack, Tag},
    {ok, Request, State};
info(eof, Request, State) ->
    {stop, Request, State};
info(Message, _Request, _State) ->
    exit({unhandled_message, Message}).

%%
%% internal
%%

start_streaming(Request, ListenerId, State) ->
    eradio_stream:start_link(self(), Request, ListenerId),
    ResponseHeaders =
        #{<<"content-type">>  => <<"audio/mpeg">>,
          <<"cache-control">> => <<"no-cache, no-store">>,
          <<"expires">>       => <<"Fri, 1 Jan 1999 12:00:00 AM GMT">>,
          <<"pragma">>        => <<"no-cache">>,
          <<"access-control-allow-origin">> => <<"*">>},
    Request2 = cowboy_req:stream_reply(200, ResponseHeaders, Request),
    {cowboy_loop, Request2, State}.
