-module(logging).

-export([get_event/4]).
-export([get_exposure/4]).

-import(utils, [get_timestamp/0]).

get_event(User, EventName, Value, Metadata) ->
  Event =
    #{
      <<"eventName">> => EventName,
      <<"metadata">> => Metadata,
      <<"user">> => User,
      <<"time">> => list_to_binary(get_timestamp())
    },
  if
    Value == undefined -> undefined;
    true -> maps:put(<<"value">>, Value, Event)
  end,
  Event.


get_exposure(User, EventName, Metadata, SecondaryExposures) ->
  Event = get_event(User, EventName, undefined, Metadata),
  maps:put(<<"secondaryExposures">>, SecondaryExposures, Event),
  Event.
