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
    eradio_stream:start_link(self()),
    ResponseHeaders =
        #{<<"content-type">>  => <<"audio/mpeg">>,
          <<"cache-control">> => <<"no-cache, no-store">>,
          <<"expires">>       => <<"Fri, 1 Jan 1999 12:00:00 AM GMT">>,
          <<"pragma">>        => <<"no-cache">>,
          <<"access-control-allow-origin">> => <<"*">>},
    Request2 = cowboy_req:stream_reply(200, ResponseHeaders, Request),
    {cowboy_loop, Request2, State}.

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
