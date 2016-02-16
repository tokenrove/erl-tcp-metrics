-module('tcp_metrics_sup').
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {#{strategy => one_for_one},
          [#{id => tcp_metrics,
             start => {tcp_metrics, start_link, []},
             restart => permanent}]}}.

