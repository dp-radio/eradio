-module(eradio_api_handler).

-include_lib("kernel/include/logger.hrl").
-include("eradio_source.hrl").

%% cowboy handler callbacks
-export([init/2]).

%%
%% cowboy handler callbacks
%%

init(Request, State) ->
    ResponseHeaders =
        #{<<"content-type">>  => <<"application/json">>,
          <<"cache-control">> => <<"no-cache, no-store">>,
          <<"expires">>       => <<"Fri, 1 Jan 1999 12:00:00 AM GMT">>,
          <<"pragma">>        => <<"no-cache">>,
          <<"access-control-allow-origin">> => <<"*">>},
    QueryString = maps:from_list(cowboy_req:parse_qs(Request)),
    #{method := Method, path := Path} = Request,
    case handle_request(Request, QueryString) of
        {ok, ResponseBody} -> {ok, cowboy_req:reply(200, ResponseHeaders, ResponseBody, Request), State};
        {websocket, Arg}   -> {eradio_websocket_handler, Request, Arg};
        not_found          -> {ok, cowboy_req:reply(404, ResponseHeaders, <<>>, Request), State};

        {invalid, InvalidReason} ->
            ?LOG_INFO("invalid ~s ~s request: ~1000p", [Method, Path, InvalidReason]),
            {ok, cowboy_req:reply(400, #{}, <<>>, Request), State}
    end.

handle_request(#{path := <<"/v1/ws/notify">>}, _QueryString) ->
    {websocket, eradio_websocket:start_notify_opts()};

handle_request(#{path := <<"/v1/metadata">>, method := <<"GET">>}, _QueryString) ->
    CurrentTrackMap = case eradio_source:player_state() of
                          {ok, PlayerState} -> current_track_response(PlayerState);
                          _                 -> #{}
                      end,
    {ok, {Listeners, Vetoes}} = eradio_source:vetoes(),
    {ok, jsone:encode(metadata_response(CurrentTrackMap, length(Listeners), map_size(Vetoes)))};

handle_request(#{path := <<"/v1/veto">>, method := <<"PUT">>}, #{<<"listener_id">> := <<ListenerIdBin/binary>>}) ->
    try erlang:binary_to_integer(ListenerIdBin) of
        ListenerId ->
            eradio_source:veto(ListenerId),
            {ok, <<"{}">>}
    catch error:Reason -> {invalid, Reason} end;

handle_request(_Request, _QueryString) ->
    not_found.

metadata_response(CurrentTrackMap, ListenerCount, VetoCount) ->
    CurrentTrackMap#{listener_count => ListenerCount, veto_count => VetoCount}.

current_track_response({playing, #track{uri = Uri, name = Name}}) ->
    #{current_track => #{uri => Uri, name => Name}};
current_track_response({playing, unknown}) ->
    #{};
current_track_response(stopped) ->
    #{}.
