-module(logging).

-export([get_event/4]).
-export([get_exposure/4]).

get_event(User, EventName, Value, Metadata) ->
  PublicUser = maps:remove(<<"privateAttributes">>, User),
  Event =
    #{
      <<"eventName">> => EventName,
      <<"metadata">> => Metadata,
      <<"user">> => PublicUser,
      <<"time">> => list_to_integer(utils:get_timestamp())
    },
  case Value == undefined of
    true -> Event;
    _HasValue -> maps:put(<<"value">>, Value, Event)
  end.


get_exposure(User, EventName, Metadata, SecondaryExposures) ->
  Event = get_event(User, EventName, undefined, Metadata),
  maps:put(<<"secondaryExposures">>, SecondaryExposures, Event).
