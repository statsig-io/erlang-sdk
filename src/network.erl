-module(network).

-export([request/3]).

normalize_api() ->
  Url = application:get_env(statsig, statsig_api, "https://statsigapi.net/v1/"),
  TrailingSlash = string:find(Url, "/", trailing),
  case TrailingSlash of
    "/" -> Url;
    _Other -> Url ++ "/"
  end.


request(ApiKey, Endpoint, Input) ->
  Api = normalize_api(),
  Headers =
    [
      {"STATSIG-API-KEY", ApiKey},
      {"STATSIG-CLIENT-TIME", utils:get_timestamp()},
      {"STATSIG-SDK-TYPE", utils:get_sdk_type()},
      {"STATSIG-SDK-VERSION", utils:get_sdk_version()},
      {"Content-Type", <<"application/json">>}
    ],
  RequestBody =
    jiffy:encode(
      maps:put(<<"statsigMetadata">>, utils:get_statsig_metadata(), Input)
    ),
  case hackney:post(Api ++ Endpoint, Headers, RequestBody, []) of
    {ok, StatusCode, _RespHeaders, ClientRef} ->
      if
        StatusCode < 300 ->
          {ok, Body} = hackney:body(ClientRef),
          Body;

        true -> false
      end;

    {error, _} -> false;
    true -> false
  end.
