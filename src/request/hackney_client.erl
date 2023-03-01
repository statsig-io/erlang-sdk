-module(hackney_client).

-behavior(http_client).

-export([request/4]).

request(Method, Url, ReqBody, ReqHeaders) ->
  case hackney:request(Method, Url, ReqHeaders, ReqBody, []) of
    {ok, StatusCode, RespHeaders, ClientRef} ->
      if
        StatusCode < 300 ->
          {ok, Body} = hackney:body(ClientRef),
          {ok, [{status_code, StatusCode}, {headers, RespHeaders}, {body, Body }]};

        true -> {error, [{reason, "Failed with status code"}]}
      end;
    {error, Reason} ->
      {error, [{reason, Reason}]}
  end.
  