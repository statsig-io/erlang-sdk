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
    flush/0
  ]
).

start(_Type, _Args) -> statsig_sup:start_link().

-spec check_gate(map(), binary()) -> boolean().
check_gate(User, Gate) -> 
  NormalizedUser = normalize_user(User),
  gen_server:call(statsig_server, {gate, NormalizedUser, Gate}).

-spec get_config(map(), binary()) -> map().
get_config(User, Config) ->
  NormalizedUser = normalize_user(User),
  gen_server:call(statsig_server, {config, NormalizedUser, Config}).

-spec get_experiment(map(), binary()) -> map().
get_experiment(User, Experiment) ->
  NormalizedUser = normalize_user(User),
  gen_server:call(statsig_server, {config, NormalizedUser, Experiment}).

-spec log_event(map(), binary(), map()) -> none().
log_event(User, EventName, Metadata) ->
  NormalizedUser = normalize_user(User),
  gen_server:cast(statsig_server, {log, NormalizedUser, EventName, undefined, Metadata}).

-spec log_event(map(), binary(), binary() | number(), map()) -> none().
log_event(User, EventName, Value, Metadata) ->
  NormalizedUser = normalize_user(User),
  gen_server:cast(statsig_server, {log, NormalizedUser, EventName, Value, Metadata}).

normalize_user(User) ->
  env = application:get_env(statsig, statsig_environment_tier, undefined),
  case env of
    undefined ->
      User;
    Other ->
      StatsigEnvironment = #{<<"tier">> => env},
      User = maps:put("statsigEnvironment", StatsigEnvironment, User)
  end.

-spec flush() -> none().
flush() -> gen_server:cast(statsig_server, flush).

stop(_State) -> ok.
