-module(statsig).

-behaviour(application).

-export(
  [
    start/2,
    stop/1,
    check_gate/2,
    get_config/2,
    get_experiment/2,
    log_event/3,
    log_event/4,
    flush/0,
    flush_sync/0
  ]
).

start(_Type, _Args) -> statsig_sup:start_link().

-spec check_gate(map(), binary()) -> boolean().
check_gate(User, Gate) ->
  NormalizedUser = utils:get_user_with_environment(User),
  gen_server:call(statsig_server, {gate, NormalizedUser, Gate}).


-spec get_config(map(), binary()) -> map().
get_config(User, Config) ->
  NormalizedUser = utils:get_user_with_environment(User),
  gen_server:call(statsig_server, {config, NormalizedUser, Config}).


-spec get_experiment(map(), binary()) -> map().
get_experiment(User, Experiment) ->
  NormalizedUser = utils:get_user_with_environment(User),
  gen_server:call(statsig_server, {config, NormalizedUser, Experiment}).


-spec log_event(map(), binary(), map()) -> ok.
log_event(User, EventName, Metadata) ->
  NormalizedUser = utils:get_user_with_environment(User),
  gen_server:cast(
    statsig_server,
    {log, NormalizedUser, EventName, undefined, Metadata}
  ),
  ok.


-spec log_event(map(), binary(), binary() | number(), map()) -> ok.
log_event(User, EventName, Value, Metadata) ->
  NormalizedUser = utils:get_user_with_environment(User),
  gen_server:cast(
    statsig_server,
    {log, NormalizedUser, EventName, Value, Metadata}
  ),
  ok.


-spec flush() -> ok.
flush() ->
  gen_server:cast(statsig_server, {flush}),
  ok.


-spec flush_sync() -> ok.
flush_sync() ->
  gen_server:call(statsig_server, {flush}),
  ok.

stop(_State) -> ok.
