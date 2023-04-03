-module(consistency_test).

-include_lib("eunit/include/eunit.hrl").

gate_test() ->
  ApiKey = os:getenv("test_api_key"),
  application:set_env(statsig, statsig_api_key, ApiKey),
  application:set_env(statsig, statsig_api, "https://api.statsig.com/v1"),
  application:start(statsig),
  {ok, _Apps} = application:ensure_all_started(statsig),
  
  Body = network:request(ApiKey, "rulesets_e2e_test", #{}),

  TestData =  maps:get(<<"data">>, jiffy:decode(Body, [return_maps]), []),
  lists:map(fun (Data) -> test_input(Data) end, TestData),
  application:set_env(statsig, statsig_environment_tier, <<"staging">>),
  statsig:log_event(#{<<"userID">> => <<"321">>}, <<"newevent">>, 12, #{<<"test">> => <<"val">>}),
  statsig:log_event(#{<<"userID">> => <<"456">>}, <<"custom_event">>, <<"hello">>, #{<<"123">> => <<"444">>}),
  statsig:log_event(#{<<"userID">> => <<"12345">>}, <<"custom_event">>, #{<<"test">> => <<"val">>}),
  statsig:flush(),
  application:stop(statsig).

test_input(Input) ->
  User = maps:get(<<"user">>, Input, #{}),
  FeatureGates = maps:get(<<"feature_gates_v2">>, Input, #{}),
  maps:map(fun (K, V) -> test_gate(K, V, User) end, FeatureGates),
  DynamicConfigs = maps:get(<<"dynamic_configs">>, Input, #{}),
  maps:map(fun (K, V) -> test_config(K, V, User) end, DynamicConfigs).

test_gate(Name, Gate, User) ->
  if
    Name == <<"test_country">> ->
      false;
    Name == <<"test_id_list">> ->
      false;
    Name == <<"test_not_in_id_list">> ->
      false;
    Name == <<"test_ua_os">> ->
      false;
    Name == <<"test_ua">> ->
      false;
    Name == <<"test_windows_7">> ->
      false;
    true ->
      Result = statsig:check_gate(User, Name),
      ServerResult = maps:get(<<"value">>, Gate, false),
      ?assertEqual(Result, ServerResult)
  end.

test_config(Name, Config, User) ->
  if
    Name == <<"operating_system_config">> ->
      false;
    true ->
      ResultConfig = statsig:get_config(User, Name),
      ResultValue = maps:get(value, ResultConfig, #{}),
      ServerResult = maps:get(<<"value">>, Config, false),
      ?assertEqual(ResultValue, ServerResult)
  end.
  