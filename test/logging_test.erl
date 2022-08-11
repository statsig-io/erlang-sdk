-module(logging_test).

-include_lib("eunit/include/eunit.hrl").

private_attribute_test() ->
  PrivateUser = #{<<"userID">> => <<"jkw">>, <<"privateAttributes">> => #{<<"gb">> => <<"goodbye">>}},
  Event = logging:get_event(PrivateUser, <<"test_event">>, 42, #{}),
  EventUser = maps:get(<<"user">>, Event, undefined),
  
  ?assert(maps:get(<<"privateAttributes">>, EventUser, undefined) == undefined),

  PublicUser = #{<<"userID">> => <<"jkw">>, <<"email">> => <<"hello@statsig.com">>},
  Event2 = logging:get_event(PublicUser, <<"test_event">>, 42, #{}),
  EventUser2 = maps:get(<<"user">>, Event2, undefined),
  ?assert(maps:get(<<"privateAttributes">>, EventUser2, undefined) == undefined),
  ?assert(maps:get(<<"email">>, EventUser2, <<"">>) == <<"hello@statsig.com">>).
