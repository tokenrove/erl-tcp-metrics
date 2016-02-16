-module(tcp_metrics).

-export([start_link/0,
         init/1,
         lookup/1]).

-define(TABLE, tcp_metrics_ip_to_rtt).

-type ip() :: 0..4294967295.
-type rtt() :: integer().

%%% PUBLIC

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> proc_lib:start_link(?MODULE, init, [self()]).


-spec init(pid()) -> ok.
init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    ets:new(?TABLE, [set, {read_concurrency, true}, named_table]),
    loop(create_port(), <<>>).


%% lookup the estimated RTT in usecs for an IP.
-spec lookup(ip() |
             {0..255,0..255,0..255,0..255} |
             binary())
            -> rtt() | undefined.
lookup(Ip) when is_integer(Ip) ->
    case ets:lookup(?TABLE, Ip) of
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


-spec create_port() -> port().
create_port() ->
    open_port({spawn, port_path("tcp_metrics_port")},
              [in, binary, exit_status]).


-spec loop(port(), binary()) -> ok.
loop(Port, Buffer) ->
    receive
        {_Port, {data, In}} ->
            loop(Port, process(<<Buffer/bytes, In/bytes>>));
        {_Port, {exit_status, _Status}} ->
            loop(create_port(), <<>>);
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
