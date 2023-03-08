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
  ClientModule = application:get_env(statsig, http_client, hackney_client),
  RequestHeaders =
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

  case ClientModule:request(post, Api ++ Endpoint, RequestBody, RequestHeaders) of
    {ok, #{status_code := StatusCode, body := Body}} ->
      if
        StatusCode < 300 ->
          Body;
        true -> false
      end;

    {error, _} -> false;
    true -> false
  end.
