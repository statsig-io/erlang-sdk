-module(utils).

-export(
  [get_timestamp/0, get_sdk_type/0, get_sdk_version/0, get_statsig_metadata/0]
).

-spec get_timestamp() -> list().
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  integer_to_list((Mega * 1000000 + Sec) * 1000 + round(Micro / 1000)).


-spec get_statsig_metadata() -> map().
get_statsig_metadata() ->
  #{<<"sdkType">> => get_sdk_type(), <<"sdkVersion">> => get_sdk_version()}.

-spec get_sdk_version() -> list().
get_sdk_version() -> "0.1.0".

-spec get_sdk_type() -> list().
get_sdk_type() -> "erlang".
