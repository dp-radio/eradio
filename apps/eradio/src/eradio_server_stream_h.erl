-module(eradio_server_stream_h).
-behaviour(cowboy_stream).
-on_load(init_module/0).

-include_lib("kernel/include/logger.hrl").

%% public API
-export([socket_out_queue/2]).

%% cowboy_stream callbacks
-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-record(state,
        {next   :: any(),
         sock   :: port(),
         sndbuf :: pos_integer()}).

%%
%% public API
%%

-spec socket_out_queue(cowboy_req:req(), non_neg_integer()) -> {ok, {integer(), integer()}} | {error, any()}.
socket_out_queue(Request, Timeout) ->
    Tag = make_ref(),
    ok = cowboy_req:cast({socket_out_queue, {self(), Tag}}, Request),
    receive
        {Tag, Reply} -> Reply
    after
        Timeout -> {error, timeout}
    end.

%%
%% cowboy_stream callbacks
%%

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts()) -> {cowboy_stream:commands(), any()}.
init(StreamId, Req, Opts) ->
    Socket = maps:get(eradio_sock, Opts),
    SendBuffer = get_send_buffer(Socket),
    {NextCommands, Next} = cowboy_stream:init(StreamId, Req, Opts),
    {NextCommands, #state{next = Next, sock = Socket, sndbuf = SendBuffer}}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), binary(), State) -> {cowboy_stream:commands(), State}.
data(StreamId, IsFin, Data, State) ->
    {NextCommands, NewNext} = cowboy_stream:data(StreamId, IsFin, Data, State#state.next),
    {NextCommands, State#state{next = NewNext}}.

-spec info(cowboy_stream:streamid(), any(), State) -> {cowboy_stream:commands(), State}.
info(StreamId, Info, State) ->
    {Commands, NewState} = handle_info(StreamId, Info, State),
    {NextCommands, NewNext} = cowboy_stream:info(StreamId, Info, State#state.next),
    {Commands ++ NextCommands, NewState#state{next = NewNext}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), any()) -> any().
terminate(StreamId, Reason, State) ->
    cowboy_stream:terminate(StreamId, Reason, State#state.next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(), cowboy_req:partial_req(), Resp, cowboy:opts())
                 -> Resp when Resp::cowboy_stream:resp_command().
early_error(StreamId, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamId, Reason, PartialReq, Resp, Opts).

%%
%% private
%%

init_module() ->
    case erlang:load_nif(nif_lib_path(), 0) of
        ok -> ok;
        {error, LoadNifError} ->
            ?LOG_WARNING("error loading nif library: ~1000p", [LoadNifError])
    end.

nif_lib_path() ->
    App = eradio_app:application(),
    case code:priv_dir(App) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv, App])) of
                true -> filename:join(["..", priv, App]);
                false -> filename:join([priv, App])
            end;
        AppPrivDir -> filename:join([AppPrivDir, App])
    end.

get_send_buffer(Socket) ->
    case inet:getopts(Socket, [sndbuf]) of
        {ok, [{sndbuf, SendBuffer}]} -> SendBuffer;
        {error, einval} -> exit(normal)
    end.

handle_info(_StreamId, {socket_out_queue, {From, Tag}}, State) ->
    Reply = do_socket_out_queue(State),
    From ! {Tag, Reply},
    {[], State};
handle_info(_StreamId, _Info, State) ->
    {[], State}.

do_socket_out_queue(State) ->
    try
        case prim_inet:getfd(State#state.sock) of
            {ok, Fd} -> nif_socket_out_queue(Fd);
            {error, GetFdError} -> {error, GetFdError}
        end
    of
        {ok, SocketOutQueue} -> {ok, {SocketOutQueue, State#state.sndbuf}};
        {error, Error} -> {error, Error}
    catch
        _:Error -> {error, Error}
    end.

nif_socket_out_queue(_Fd) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).
