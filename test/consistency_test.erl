-module(consistency_test).

-include_lib("eunit/include/eunit.hrl").

gate_test() ->
  ApiKey = os:getenv("test_api_key"),
  application:set_env(statsig, statsig_api_key, ApiKey),
  application:start(statsig),

  Body = network:request(ApiKey, "rulesets_e2e_test", #{}, true),
  TestData =  maps:get(<<"data">>, jiffy:decode(Body, [return_maps]), []),
  lists:map(fun (Data) -> test_input(Data) end, TestData),
  statsig:log_event(#{<<"userID">> => <<"321">>}, <<"custom_event">>, 12, #{<<"test">> => <<"val">>}),
  statsig:log_event(#{<<"userID">> => <<"456">>}, <<"custom_event">>, <<"hello">>, #{<<"123">> => <<"444">>}),
  statsig:log_event(#{<<"userID">> => <<"12345">>}, <<"custom_event">>, #{<<"test">> => <<"val">>}),
  statsig:flush().

test_input(Input) ->
  User = maps:get(<<"user">>, Input, #{}),
  FeatureGates = maps:get(<<"feature_gates_v2">>, Input, #{}),
  maps:map(fun (K, V) -> test_gate(Pid, K, V, User) end, FeatureGates),
  DynamicConfigs = maps:get(<<"dynamic_configs">>, Input, #{}),
  maps:map(fun (K, V) -> test_config(Pid, K, V, User) end, DynamicConfigs).

test_gate(Name, Gate, User) ->
  if
    Name == <<"test_country">> ->
      false;
    Name == <<"test_id_list">> ->
      false;
    Name == <<"test_not_in_id_list">> ->
      false;
    Name == <<"test_time_before">> ->
      false;
    Name == <<"test_time_after">> ->
      false;
    Name == <<"test_time_before_string">> ->
      false;
    Name == <<"test_ua_os">> ->
      false;
    Name == <<"test_ua">> ->
      false;
    Name == <<"test_time_on">> ->
      false;
    Name == <<"test_time_before">> ->
      false;
    true ->
      Result = statsig:check_gate(User, Name),
      ServerResult = maps:get(<<"value">>, Gate, false),
      ?assert(Result == ServerResult, Name)
  end.

test_config(Pid, Name, Config, User) ->
  if
    Name == <<"operating_system_config">> ->
      false;
    true ->
      Result = statsig:get_config(Pid, User, Name),
      ServerResult = maps:get(<<"value">>, Config, false),
      ?assert(Result == ServerResult, Name)
  end.
  