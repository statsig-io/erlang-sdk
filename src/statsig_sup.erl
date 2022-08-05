-module(statsig_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, shutdown/0]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  case application:get_env(statsig, statsig_api_key) of
    {ok, ApiKey} ->
      RestartStrategy = {one_for_one, 10, 60},
      Server =
        {
          stasig_serv,
          {statsig_server, start_link, [ApiKey]},
          permanent,
          infinity,
          worker,
          [statsig_server]
        },
      Children = [Server],
      {ok, {RestartStrategy, Children}};

    Other -> {error, Other}
  end.


shutdown() -> exit(whereis(?MODULE), shutdown).
