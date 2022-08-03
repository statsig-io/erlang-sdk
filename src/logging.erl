-module(logging).

-export([get_event/4]).
-export([get_exposure/4]).

get_event(User, EventName, Value, Metadata) ->
  Event =
    #{
      <<"eventName">> => EventName,
      <<"metadata">> => Metadata,
      <<"user">> => User,
      <<"time">> => list_to_binary(utils:get_timestamp())
    },
  if
    Value == undefined -> Event;
    true -> maps:put(<<"value">>, Value, Event)
  end.


get_exposure(User, EventName, Metadata, SecondaryExposures) ->
  Event = get_event(User, EventName, undefined, Metadata),
  maps:put(<<"secondaryExposures">>, SecondaryExposures, Event).
