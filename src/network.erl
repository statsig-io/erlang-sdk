-module(network).

-export([request/4]).

request(ApiKey, Endpoint, Input, Sync) ->
  Method = post,
  URL = application:get_env(statsig, statsig_api, "https://statsigapi.net/v1/"),
  Header =
    [
      {"STATSIG-API-KEY", ApiKey},
      {"STATSIG-CLIENT-TIME", utils:get_timestamp()},
      {"STATSIG-SDK-TYPE", utils:get_sdk_type()},
      {"STATSIG-SDK-VERSION", utils:get_sdk_version()}
    ],
  Type = "application/json",
  maps:put(<<"statsigMetadata">>, utils:get_statsig_metadata(), Input),
  RequestBody = jiffy:encode(Input),
  HTTPOptions = [],
  Options = [{sync, Sync}],
  case
  httpc:request(Method, {URL ++ Endpoint, Header, Type, RequestBody}, HTTPOptions, Options) of
    {ok, {{_, StatusCode, _}, _, Body}} ->
      if
        StatusCode < 300 -> Body;
        true -> false
      end;

    {ok, _RequestId} -> true;
    {error, _} -> false;
    true -> false
  end.
