-module(logging).

-export([get_event/4]).
-export([get_exposure/4]).

-import(utils, [get_timestamp/0]).

get_event(User, EventName, Value, Metadata) ->
  #{
    <<"eventName">> => EventName,
    <<"value">> => Value,
    <<"metadata">> => Metadata,
    <<"user">> => User,
    <<"time">> => get_timestamp()
  }.

get_exposure(User, EventName, Metadata, SecondaryExposures) ->
  Event = get_event(User, EventName, undefined, Metadata),
  maps:put(<<"secondaryExposures">>, SecondaryExposures, Event),
  Event.
