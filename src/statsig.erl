-module(statsig).

-behaviour(application).

-export(
  [
    start/2,
    stop/1,
    check_gate/2,
    log_event/3,
    log_event/4,
    flush/0
  ]
).


start(_Type, _Args) ->
  statsig_sup:start_link().

-spec check_gate(map(), binary()) -> boolean().
check_gate(User, Gate) -> 
  gen_server:call(statsig_server, {User, Gate}).

-spec log_event(map(), binary(), map()) -> none().
log_event(User, EventName, Metadata) ->
  gen_server:cast(statsig_server, {log, User, EventName, undefined, Metadata}).

-spec log_event(map(), binary(), binary() | number(), map()) -> none().
log_event(User, EventName, Value, Metadata) ->
  gen_server:cast(statsig_server, {log, User, EventName, Value, Metadata}).

-spec flush() -> none().
flush() -> 
  gen_server:cast(statsig_server, flush).
  
stop(_State) ->
  ok.
