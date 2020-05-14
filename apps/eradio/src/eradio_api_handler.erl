-module(eradio_api_handler).

-include_lib("kernel/include/logger.hrl").

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
    {ok, <<"{}">>};
handle_request(_Request) ->
    not_found.
