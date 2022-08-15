-module(utils).

-export(
  [
    get_timestamp/0,
    get_sdk_type/0,
    get_sdk_version/0,
    get_statsig_metadata/0,
    get_user_with_environment/1,
    get_bool_as_string/1
  ]
).

-spec get_timestamp() -> list().
get_timestamp() -> integer_to_list(os:system_time(millisecond)).

-spec get_statsig_metadata() -> map().
get_statsig_metadata() ->
  #{
    <<"sdkType">> => list_to_binary(get_sdk_type()),
    <<"sdkVersion">> => list_to_binary(get_sdk_version())
  }.

-spec get_sdk_version() -> list().
get_sdk_version() -> "0.1.0".

-spec get_sdk_type() -> list().
get_sdk_type() -> "erlang-server".

-spec get_user_with_environment(map()) -> map().
get_user_with_environment(User) ->
  case maps:is_key(<<"statsigEnvironment">>, User) of
    true -> User;
    false -> set_env(User)
  end.


set_env(User) ->
  case application:get_env(statsig, statsig_environment_tier) of
    undefined -> User;

    {ok, Tier} ->
      StatsigEnvironment = #{<<"tier">> => Tier},
      maps:put(<<"statsigEnvironment">>, StatsigEnvironment, User)
  end.

-spec get_bool_as_string(boolean()) -> list().
get_bool_as_string(Bool) ->
  case Bool of
    true -> <<"true">>;
    false -> <<"false">>
  end.