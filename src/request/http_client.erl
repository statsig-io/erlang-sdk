-module(http_client).

-type http_method() :: get | post | delete | options | head.

-callback request(
  method: http_method(),
  url: binary(),
  req_body: binary(),
  headers: list({binary, binary})
) -> {ok, {status_code: integer(), headers: any()}}
  | {ok, {status_code: integer(), headers: any(), body: binary()}}
  | {error, {reason: any()}}.

