-module(statsig).

-behaviour(gen_server).

-export(
  [
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    check_gate/3,
    get_config/3,
    log_event/4,
    log_event/5,
    flush/1
  ]
).

-import(utils, [get_timestamp/0]).
-import(network, [start/0, stop/0, request/4]).
-import(evaluator, [eval_gate/3, eval_config/3]).
-import(logging, [get_event/4, get_exposure/4]).

init([{apiKey, ApiKey}]) ->
  network:start(),
  Body = network:request(ApiKey, "download_config_specs", #{}, true),
  if
    Body == false ->
      network:stop(),
      {stop, "Initialize Failed"};

    true ->
      {
        ok,
        [
          {configSpecs, jiffy:decode(Body, [return_maps])},
          {logEvents, []},
          {apiKey, ApiKey}
        ]
      }
  end.


-spec check_gate(pid(), map(), binary()) -> boolean().
check_gate(Pid, User, Gate) -> gen_server:call(Pid, {gate, User, Gate}).

-spec get_config(pid(), map(), binary()) -> map().
get_config(Pid, User, Config) -> gen_server:call(Pid, {config, User, Config}).

-spec get_experiment(pid(), map(), binary()) -> map().
get_experiment(Pid, User, Experiment) -> gen_server:call(Pid, {config, User, Experiment}).

-spec log_event(pid(), map(), binary(), map()) -> none().
log_event(Pid, User, EventName, Metadata) ->
  gen_server:cast(Pid, {log, User, EventName, undefined, Metadata}).

-spec log_event(pid(), map(), binary(), binary() | number(), map()) -> none().
log_event(Pid, User, EventName, Value, Metadata) ->
  gen_server:cast(Pid, {log, User, EventName, Value, Metadata}).

handle_cast(
  {log, User, EventName, Value, Metadata},
  [{configSpecs, ConfigSpecs}, {logEvents, Events}, {apiKey, ApiKey}]
) ->
  Event = logging:get_event(User, EventName, Value, Metadata),
  {
    noreply,
    [
      {configSpecs, ConfigSpecs},
      {logEvents, [Event | Events]},
      {apiKey, ApiKey}
    ]
  };

handle_cast(
  flush,
  [{configSpecs, _ConfigSpecs}, {logEvents, Events}, {apiKey, ApiKey}]
) ->
  flush_events(ApiKey, Events),
  {noreply, [{configSpecs, _ConfigSpecs}, {logEvents, []}, {apiKey, ApiKey}]}.


-spec flush(pid()) -> none().
flush(Pid) -> gen_server:cast(Pid, flush).

handle_info(In, State) -> {noreply, State}.

handle_call(
  {Type, User, Name},
  _From,
  State
) ->
  if Type == gate ->
    handle_gate(User, Name, State);
  true ->
    handle_config(User, Name, State)
  end.

handle_gate(
  User,
  Gate,
  [{configSpecs, ConfigSpecs}, {logEvents, Events}, {apiKey, ApiKey}]
) ->
  {_Rule, GateValue, _JsonValue, RuleID, SecondaryExposures} =
    evaluator:eval_gate(User, ConfigSpecs, Gate),
  GateExposure =
    logging:get_exposure(
      User,
      <<"statsig::gate_exposure">>,
      #{
        <<"gate">> => Gate,
        <<"gateValue">> => GateValue,
        <<"ruleID">> => RuleID
      },
      SecondaryExposures
    ),
  NextEvents = handle_events([GateExposure | Events], ApiKey),
  {
    reply,
    GateValue,
    [{configSpecs, ConfigSpecs}, {logEvents, NextEvents}, {apiKey, ApiKey}]
  }.

handle_config(
  User,
  Config,
  [{configSpecs, ConfigSpecs}, {logEvents, Events}, {apiKey, ApiKey}]
) ->
  {_Rule, _BooleanValue, ConfigValue, RuleID, SecondaryExposures} =
    evaluator:eval_config(User, ConfigSpecs, Config),
  ConfigExposure =
    logging:get_exposure(
      User,
      <<"statsig::config_exposure">>,
      #{
        <<"gate">> => Config,
        <<"ruleID">> => RuleID
      },
      SecondaryExposures
    ),
  NextEvents = handle_events([ConfigExposure | Events], ApiKey),
  {
    reply,
    ConfigValue,
    [{configSpecs, ConfigSpecs}, {logEvents, NextEvents}, {apiKey, ApiKey}]
  }.


flush_events(ApiKey, Events) ->
  Input = #{<<"events">> => Events},
  network:request(ApiKey, "rgstr", Input, false) /= false.


handle_events(Events, ApiKey) ->
  if
    length(Events) > 999 ->
      % We've been failing to post events for too long
      % lets keep the most recent 500 events
      lists:sublist(Events, 499);

    length(Events) > 499 ->
      Success = flush_events(ApiKey, Events),
      if
        Success -> [];
        true -> Events
      end;

    true -> Events
  end.


terminate(
  _Reason,
  [{configSpecs, _ConfigSpecs}, {logEvents, Events}, {apiKey, ApiKey}]
) ->
  if
    length(Events) > 0 -> flush_events(ApiKey, Events);
    true -> false
  end,
  network:stop(),
  ok.
