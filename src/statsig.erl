-module(statsig).

-behaviour(application).

-export(
  [
    start_link/0,
    start/2,
    stop/1,
    check_gate/2,
    log_event/3,
    log_event/4,
    flush/0
  ]
).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

start(_Type, _Params) ->
  {ok, ApiKey} = application:get_env(statsig, statsig_api_key),
  {ok, Pid} = gen_server:start(statsig_server, [{api_key, ApiKey}], []),
  register(statsigsdk, Pid).

-spec check_gate(map(), binary()) -> boolean().
check_gate(User, Gate) -> 
  Pid = whereis(statsigsdk),
  gen_server:call(Pid, {User, Gate}).

-spec log_event(map(), binary(), map()) -> none().
log_event(User, EventName, Metadata) ->
  Pid = whereis(statsigsdk),
  gen_server:cast(Pid, {log, User, EventName, undefined, Metadata}).

-spec log_event(map(), binary(), binary() | number(), map()) -> none().
log_event(User, EventName, Value, Metadata) ->
  Pid = whereis(statsigsdk),
  gen_server:cast(Pid, {log, User, EventName, Value, Metadata}).

-spec flush() -> none().
flush() -> 
  Pid = whereis(statsigsdk),
  gen_server:cast(Pid, flush).
  
stop(_State) ->
  Pid = whereis(statsigsdk),
  gen_server:stop(Pid),
  ok.
