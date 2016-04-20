-module(tcp_metrics).

-export([start_link/0,
         init/1,
         lookup/1]).

-define(TABLE, tcp_metrics_ip_to_rtt).

-type ip() :: 0..4294967295.
-type rtt() :: integer().

-record(state, {
    source_address :: inet:ip4_address(),
    port :: port(),
    buffer :: binary()
}).

%%% PUBLIC

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).


-spec init(pid()) -> ok.
init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    ets:new(?TABLE, [set, {read_concurrency, true}, named_table]),
    SourceAddress =
        case application:get_env(tcp_metrics, source_address) of
            {ok, X} -> X;
            _ -> {0,0,0,0}
        end,
    loop(#state{source_address = SourceAddress}).


%% lookup the estimated RTT in millisecs for an IP.
-spec lookup(ip() |
             inet:ip4_address() |
             binary())
            -> rtt() | undefined.
lookup(Ip) when is_integer(Ip) ->
    case try ets:lookup(?TABLE, Ip)
         catch error:badarg -> []
         end
    of
        [{_Ip, Rtt}] -> Rtt;
        [] -> undefined
    end;
lookup({A, B, C, D}) ->
    lookup((A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D);
lookup(<<A, B, C, D>>) ->
    lookup((A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D).


%%% PRIVATE

-spec port_path(string()) -> string().
port_path(Name) ->
    filename:join([priv_dir(), Name]).


-spec priv_dir() -> string().
priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.


-spec string_of_ip4_address(inet:ip4_address()) -> string().
string_of_ip4_address(A) ->
    string:join([integer_to_list(O) || O <- tuple_to_list(A)], ".").


-spec create_port(inet:ip4_address()) -> port().
create_port(SourceAddress) ->
    Args = ["5", string_of_ip4_address(SourceAddress)],
    open_port({spawn_executable, port_path("tcp_metrics_port")},
              [binary, exit_status, {args, Args}]).


-spec loop(#state{}) -> ok.
loop(#state{port = undefined, source_address = SourceAddress} = State) ->
    loop(State#state{port = create_port(SourceAddress), buffer = <<>>});
loop(#state{port = Port,
            buffer = Buffer} = State) ->
    receive
        {_Port, {data, In}} ->
            loop(State#state{buffer = process(<<Buffer/bytes, In/bytes>>)});
        {_Port, {exit_status, _Status}} ->
            loop(State#state{port = undefined, buffer = undefined});
        stop ->
            port_close(Port),
            ok
    end.


-spec process(binary()) -> binary().
process(<<Msg:8/bytes, Rest/bytes>>) ->
    update(decode(Msg)),
    process(Rest);
process(Rest) -> Rest.


-spec decode(binary()) -> {ip(), rtt()}.
decode(<<Ip:32/big-integer, Rtt:32/native-integer>>) ->
    {Ip, Rtt}.


-spec update({ip(), rtt()}) -> boolean().
update({Ip, Rtt}) ->
    ets:insert(?TABLE, {Ip, Rtt}).
