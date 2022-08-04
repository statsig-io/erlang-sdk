-module(network).

-export([request/3]).

request(ApiKey, Endpoint, Input) ->
  URL = application:get_env(statsig, statsig_api, "https://statsigapi.net/v1/"),
  Headers =
    [
      {<<"STATSIG-API-KEY">>, ApiKey},
      % {"STATSIG-CLIENT-TIME", get_timestamp()},
      % {"STATSIG-SDK-TYPE", get_sdk_type()},
      % {"STATSIG-SDK-VERSION", get_sdk_version()},
      {<<"Content-Type">>, <<"application/json">>}
    ],
  maps:put(<<"statsigMetadata">>, utils:get_statsig_metadata(), Input),
  RequestBody = jiffy:encode(Input),
  case hackney:post(URL ++ Endpoint, Headers, RequestBody, []) of
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