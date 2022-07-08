-module(network).

-export([start/0, stop/0, request/3]).

-import(
  utils,
  [get_timestamp/0, get_sdk_type/0, get_sdk_version/0, get_statsig_metadata/0]
).

start() ->
  inets:start(),
  ssl:start().


stop() ->
  inets:stop(),
  ssl:stop().


request(ApiKey, Endpoint, Input) ->
  Method = post,
  URL = "https://statsigapi.net/v1/" ++ Endpoint,
  Header =
    [
      {"STATSIG-API-KEY", ApiKey},
      {"STATSIG-CLIENT-TIME", get_timestamp()},
      {"STATSIG-SDK-TYPE", get_sdk_type()},
      {"STATSIG-SDK-VERSION", get_sdk_version()}
    ],
  Type = "application/json",
  maps:put(<<"statsigMetadata">>, get_statsig_metadata, Input),
  RequestBody = jiffy:encode(Input),
  HTTPOptions = [],
  Options = [],
  {ok, {{_, StatusCode, _}, _, Body}} =
    httpc:request(
      Method,
      {URL, Header, Type, RequestBody},
      HTTPOptions,
      Options
    ),
  if
    StatusCode < 300 -> Body;
    true -> false
  end.
