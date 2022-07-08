-module(consistency_test).

-include_lib("eunit/include/eunit.hrl").

-import(statsig, [check_gate/3, log_event/4, flush/1]).
-import(network, [request/3]).
-import(os, [getenv/1]).

gate_test() ->
  ApiKey = getenv("test_api_key"),
  Body = request(ApiKey, "rulesets_e2e_test", #{}),
  {ok, Pid} = gen_server:start(statsig, [{apiKey, ApiKey}], []),
  TestData =  maps:get(<<"data">>, jiffy:decode(Body, [return_maps]), []),
  lists:map(fun (Data) -> test_input(Pid, Data) end, TestData).

test_input(Pid, Input) ->
  User = maps:get(<<"user">>, Input, #{}),
  FeatureGates = maps:get(<<"feature_gates_v2">>, Input, #{}),
  maps:map(fun (K, V) -> test_gate(Pid, K, V, User) end, FeatureGates).

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
    Name == <<"test_not_in_id_list">> ->
      false;
    Name == <<"test_ua_os">> ->
      false;
    Name == <<"test_ua">> ->
      false;
    true ->
      Result = statsig:check_gate(Pid, User, Name),
      ServerResult = maps:get(<<"value">>, Gate, false),
      ?assert(Result == ServerResult)
  end.
  