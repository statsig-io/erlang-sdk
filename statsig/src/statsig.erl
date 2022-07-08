-module(statsig).

-behaviour(gen_server).

-export(
  [
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    check_gate/3,
    log_event/4,
    log_event/5,
    flush/1
  ]
).

-import(utils, [get_timestamp/0]).
-import(network, [start/0, stop/0, request/3]).
-import(evaluator, [eval_gate/3]).
-import(logging, [get_event/4, get_exposure/4]).

% Usage:
% {ok, Pid} = gen_server:start(statsig, [{apiKey, "secret-"}], []).
% statsig:check_gate(Pid, #{<<"userID">> => <<"1234">>}, <<"test">>).
% statsig:check_gate(Pid, #{<<"userID">> => <<"12345">>}, <<"test">>).
% statsig:log_event(Pid, #{<<"userID">> => <<"12345">>}, <<"custom_event">>, 12, #{<<"test">> => <<"val">>}).
% statsig:flush(Pid).
init([{apiKey, ApiKey}]) ->
  network:start(),
  Body = network:request(ApiKey, "download_config_specs", #{}),
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


check_gate(Pid, User, Gate) -> gen_server:call(Pid, {User, Gate}).

log_event(Pid, User, EventName, Metadata) ->
  gen_server:cast(Pid, {log, User, EventName, undefined, Metadata}).

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
  NewEvents = flush_events(ApiKey, Events),
  {
    noreply,
    [{configSpecs, _ConfigSpecs}, {logEvents, NewEvents}, {apiKey, ApiKey}]
  }.


flush(Pid) -> gen_server:cast(Pid, flush).

handle_call(
  {User, Gate},
  _From,
  [{configSpecs, ConfigSpecs}, {logEvents, Events}, {apiKey, ApiKey}]
) ->
  {_Rule, GateValue, RuleID, SecondaryExposures} =
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
  NextEvents = [GateExposure | Events],
  % TODO handle_events so this flushes before growing too big
  {
    reply,
    GateValue,
    [{configSpecs, ConfigSpecs}, {logEvents, NextEvents}, {apiKey, ApiKey}]
  }.


flush_events(ApiKey, Events) ->
  Input = #{<<"events">> => Events},
  network:request(ApiKey, "rgstr", Input) /= false.


handle_events(Events, ApiKey) ->
  if
    length(Events) > 1000 ->
      % We've been failing to post events for too long
      % lets keep the most recent 500 events
      lists:sublist(Events, 500);

    length(Events) > 500 ->
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
