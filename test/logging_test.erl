-module(logging_test).

-include_lib("eunit/include/eunit.hrl").

private_attribute_test() ->
  PrivateUser = #{<<"userID">> => <<"jkw">>, <<"privateAttributes">> => #{<<"gb">> => <<"goodbye">>}},
  Event = logging:get_event(PrivateUser, <<"test_event">>, 42, #{}),
  EventUser = maps:get(<<"user">>, Event, undefined),
  
  ?assert(maps:get(<<"privateAttributes">>, EventUser, undefined) == undefined).
