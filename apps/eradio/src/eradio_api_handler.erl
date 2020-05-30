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
    Request2 = case handle_request(Request) of
        {ok, ResponseBody} -> cowboy_req:reply(200, ResponseHeaders, ResponseBody, Request);
        not_found          -> cowboy_req:reply(404, ResponseHeaders, <<>>, Request)
    end,
    {ok, Request2, State}.

handle_request(#{path := <<"/v1/metadata">>}) ->
    CurrentTrackMap = case eradio_source:player_state() of
                          {ok, PlayerState} -> current_track_response(PlayerState);
                          _                 -> #{}
                      end,
    ListenerCount = integer_to_binary(length(eradio_stream:get_streams())),
    VetoCount = 0, %% XXX
    {ok, jsone:encode(metadata_response(CurrentTrackMap, ListenerCount, VetoCount))};
handle_request(_Request) ->
    not_found.

metadata_response(CurrentTrackMap, ListenerCount, VetoCount) ->
    CurrentTrackMap#{listener_count => ListenerCount, veto_count => VetoCount}.

current_track_response({playing, #track{id = Id, name = Name}}) ->
    #{current_track => #{id => base62:encode(Id, 22), name => Name}};
current_track_response({playing, unknown}) ->
    #{};
current_track_response(stopped) ->
    #{}.
