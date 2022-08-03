-module(statsig_server).

-behaviour(gen_server).

-export(
  [
    start_link/1,
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
  ]
).

start_link(ApiKey) ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [ApiKey], []).

init([ApiKey]) ->
  case network:request(ApiKey, "download_config_specs", #{}, true) of
    false ->
      {stop, "Initialize Failed"};
    Body ->
      {
        ok,
        [
          {config_specs, jiffy:decode(Body, [return_maps])},
          {log_events, []},
          {api_key, ApiKey}
        ]
      }
  end.

handle_cast(
  {log, User, EventName, Value, Metadata},
  [{config_specs, _ConfigSpecs}, {log_events, Events}, {api_key, ApiKey}]
) ->
  Event = logging:get_event(User, EventName, Value, Metadata),
  {
    noreply,
    [
      {config_specs, _ConfigSpecs},
      {log_events, [Event | Events]},
      {api_key, ApiKey}
    ]
  };

handle_cast(
  flush,
  [{config_specs, _ConfigSpecs}, {log_events, Events}, {api_key, ApiKey}]
) ->
  flush_events(api_key, Events),
  {noreply, [{config_specs, _ConfigSpecs}, {log_events, []}, {api_key, ApiKey}]}.

handle_info(_In, State) -> {noreply, State}.

handle_call(
  {User, Gate},
  _From,
  [{config_specs, ConfigSpecs}, {log_events, Events}, {api_key, ApiKey}]
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
  NextEvents = handle_events([GateExposure | Events], ApiKey),
  {
    reply,
    GateValue,
    [{config_specs, ConfigSpecs}, {log_events, NextEvents}, {api_key, ApiKey}]
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
  [{config_specs, _config_specs}, {log_events, Events}, {api_key, ApiKey}]
) ->
  if
    length(Events) > 0 -> flush_events(ApiKey, Events);
    true -> false
  end,
  ok.
