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
  gen_server:start_link({local, ?MODULE}, ?MODULE, [ApiKey], []).

init([ApiKey]) ->
  ets:new(
    statsig_store,
    [set, named_table, {keypos, 1}, {heir, none}, {read_concurrency, true}]
  ),
  case network:request(ApiKey, "download_config_specs", #{}) of
    false -> {stop, "Initialize Failed"};

    Body ->
      parse_and_save_specs(Body),
      Delay = application:get_env(statsig, statsig_polling_interval, 60000),
      FlushDelay = application:get_env(statsig, statsig_flush_interval, 60000),
      erlang:send_after(Delay, self(), download_specs),
      erlang:send_after(FlushDelay, self(), handle_events),
      {ok, [{log_events, []}, {api_key, ApiKey}]}
  end.


parse_and_save_specs(Body) ->
  try
    Specs = jiffy:decode(Body, [return_maps]),
    Gates = maps:get(<<"feature_gates">>, Specs, []),
    save_specs(Gates, feature_gate),
    Configs = maps:get(<<"dynamic_configs">>, Specs, []),
    save_specs(Configs, dynamic_config)
  catch
    error : _Error ->
      % no op - will be retried after the polling interval
      ok
  end.


save_specs([], _Type) -> ok;

save_specs([H | T], Type) ->
  Name = maps:get(<<"name">>, H, undefined),
  case Name of
    undefined -> ok;
    _ -> ets:insert(statsig_store, {Name, Type, H})
  end,
  save_specs(T, Type).


handle_cast(
  {log, User, EventName, Value, Metadata},
  [{log_events, Events}, {api_key, ApiKey}]
) ->
  Event = logging:get_event(User, EventName, Value, Metadata),
  {noreply, [{log_events, [Event | Events]}, {api_key, ApiKey}]};

handle_cast(flush, [{log_events, Events}, {api_key, ApiKey}]) ->
  Unsent = handle_events(Events, ApiKey),
  {noreply, [{log_events, Unsent}, {api_key, ApiKey}]}.


handle_info(download_specs, [{log_events, Events}, {api_key, ApiKey}]) ->
  case network:request(ApiKey, "download_config_specs", #{}) of
    false -> unknown;
    Body -> parse_and_save_specs(Body)
  end,
  Delay = application:get_env(statsig, statsig_polling_interval, 60000),
  erlang:send_after(Delay, self(), download_specs),
  {noreply, [{log_events, Events}, {api_key, ApiKey}]};

handle_info(flush, [{log_events, Events}, {api_key, ApiKey}]) ->
  Unsent = handle_events(Events, ApiKey),
  {noreply, [{log_events, Unsent}, {api_key, ApiKey}]};

handle_info(_In, State) -> {noreply, State}.


handle_call({Type, User, Name}, _From, State) ->
  case Type of
    gate -> handle_gate(User, Name, State);
    _Other -> handle_config(User, Name, State)
  end.


handle_gate(User, Gate, [{log_events, Events}, {api_key, ApiKey}]) ->
  {_Rule, GateValue, _JsonValue, RuleID, SecondaryExposures} =
    evaluator:find_and_eval(User, Gate, feature_gate),
  GateExposure =
    logging:get_exposure(
      User,
      <<"statsig::gate_exposure">>,
      #{
        <<"gate">> => Gate,
        <<"gateValue">> => utils:get_bool_as_string(GateValue),
        <<"ruleID">> => RuleID
      },
      SecondaryExposures
    ),
  NextEvents = [GateExposure | Events],
  {reply, GateValue, [{log_events, NextEvents}, {api_key, ApiKey}]}.


handle_config(User, Config, [{log_events, Events}, {api_key, ApiKey}]) ->
  {_Rule, _GateValue, JsonValue, RuleID, SecondaryExposures} =
    evaluator:find_and_eval(User, Config, dynamic_config),
  ConfigExposure =
    logging:get_exposure(
      User,
      <<"statsig::config_exposure">>,
      #{<<"config">> => Config, <<"ruleID">> => RuleID},
      SecondaryExposures
    ),
  NextEvents = [ConfigExposure | Events],
  {reply, #{value => JsonValue, rule_id => RuleID}, [{log_events, NextEvents}, {api_key, ApiKey}]}.


handle_events(Events, ApiKey) ->
  BatchSize = application:get_env(statsig, statsig_flush_batch_size, 500),
  BucketsOfEvents = utils:partition(Events, BatchSize),
  Unsent = unsent_events(BucketsOfEvents, ApiKey),
  lists:flatten(Unsent).

unsent_events([], _ApiKey) ->
  [];
unsent_events([HEvents|TEvents], ApiKey) -> 
  Input = #{<<"events">> => TEvents},
  case network:request(ApiKey, "rgstr", Input) of
    false ->
      [TEvents | unsent_events(HEvents, ApiKey)];
    true ->
      unsent_events(HEvents, ApiKey)
  end.
  

terminate(_Reason, [{log_events, Events}, {api_key, ApiKey}]) ->
  handle_events(Events, ApiKey),
  ok.
