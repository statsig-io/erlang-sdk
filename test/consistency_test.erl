-module(consistency_test).

-include_lib("eunit/include/eunit.hrl").

-import(statsig, [check_gate/3, get_config/3, log_event/3, log_event/4, flush/1]).
-import(network, [request/4]).
-import(os, [getenv/1]).

gate_test() ->
  ApiKey = getenv("test_api_key"),
  Body = request(ApiKey, "rulesets_e2e_test", #{}, true),
  {ok, Pid} = gen_server:start(statsig, [{apiKey, ApiKey}], []),
  TestData =  maps:get(<<"data">>, jiffy:decode(Body, [return_maps]), []),
  lists:map(fun (Data) -> test_input(Pid, Data) end, TestData),
  statsig:log_event(Pid, #{<<"userID">> => <<"321">>}, <<"custom_event">>, 12, #{<<"test">> => <<"val">>}),
  statsig:log_event(Pid, #{<<"userID">> => <<"456">>}, <<"custom_event">>, <<"hello">>, #{<<"123">> => <<"444">>}),
  statsig:log_event(Pid, #{<<"userID">> => <<"12345">>}, <<"custom_event">>, #{<<"test">> => <<"val">>}),
  statsig:flush(Pid).

test_input(Pid, Input) ->
  User = maps:get(<<"user">>, Input, #{}),
  FeatureGates = maps:get(<<"feature_gates_v2">>, Input, #{}),
  maps:map(fun (K, V) -> test_gate(Pid, K, V, User) end, FeatureGates),
  DynamicConfigs = maps:get(<<"dynamic_configs">>, Input, #{}),
  maps:map(fun (K, V) -> test_config(Pid, K, V, User) end, DynamicConfigs).

test_gate(Pid, Name, Gate, User) ->
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
    true ->
      Result = statsig:check_gate(Pid, User, Name),
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
  