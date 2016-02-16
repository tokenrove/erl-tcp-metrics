-module(tcp_metrics_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tcp_metrics_sup:start_link().

stop({ok, Pid}) ->
    Pid ! stop,
    ok.
