-module(evaluator).

-export([eval_gate/3, eval_config/3]).

-spec eval_gate(map(), map(), list()) ->
  {map(), boolean(), map(), list(), list()}.
eval_gate(User, ConfigSpecs, Gate) ->
  Gates = maps:get(<<"feature_gates">>, ConfigSpecs, []),
  ConfigSpec = find_definition(Gates, Gate),
  eval(User, ConfigSpecs, ConfigSpec).


-spec eval_config(map(), map(), list()) ->
  {map(), boolean(), map(), list(), list()}.
eval_config(User, ConfigSpecs, Config) ->
  Configs = maps:get(<<"dynamic_configs">>, ConfigSpecs, []),
  ConfigSpec = find_definition(Configs, Config),
  eval(User, ConfigSpecs, ConfigSpec).


find_definition([], _Gate) -> #{};

find_definition([H | T], Gate) ->
  Name = maps:get(<<"name">>, H, false),
  if
    Name == Gate -> H;
    true -> find_definition(T, Gate)
  end.


eval(User, ConfigSpecs, ConfigDefinition) ->
  Enabled = maps:get(<<"enabled">>, ConfigDefinition, false),
  if
    map_size(ConfigDefinition) == 0 -> {#{}, false, #{}, "", []};

    Enabled ->
      Rules = maps:get(<<"rules">>, ConfigDefinition, []),
      eval_rules(User, ConfigSpecs, Rules, ConfigDefinition);

    true ->
      {
        #{},
        false,
        maps:get(<<"defaultValue">>, ConfigDefinition, #{}),
        <<"disabled">>,
        []
      }
  end.


eval_rules(_User, _ConfigSpecs, [], Config) ->
  {#{}, false, maps:get(<<"defaultValue">>, Config, #{}), default, []};

eval_rules(User, ConfigSpecs, [Rule | Rules], Config) ->
  {RuleResult, RuleJson, RuleID, _SecondaryExposures} =
    eval_rule(User, ConfigSpecs, Rule),
  if
    RuleResult ->
      Pass = eval_pass_percent(User, Rule, Config),
      if
        Pass -> {Rule, Pass, RuleJson, RuleID, []};

        true ->
          {Rule, Pass, maps:get(<<"defaultValue">>, Config, #{}), RuleID, []}
      end;

    true -> eval_rules(User, ConfigSpecs, Rules, Config)
  end.


eval_rule(User, ConfigSpecs, Rule) ->
  Conditions = maps:get(<<"conditions">>, Rule, []),
  Results =
    lists:map(
      fun (Condition) -> eval_condition(User, ConfigSpecs, Condition) end,
      Conditions
    ),
  Pass = lists:filter(fun ({Res, _Exposures}) -> Res == false end, Results),
  if
    length(Pass) > 0 ->
      {false, maps:get(<<"returnValue">>, Rule, []), <<"fail">>, []};

    true -> {true, maps:get(<<"returnValue">>, Rule, []), <<"fail">>, []}
  end.


eval_condition(User, ConfigSpecs, Condition) ->
  {ConditionResult, EvaluationComplete, Value, _Exposures} =
    get_evaluation_value(User, ConfigSpecs, Condition),
  if
    EvaluationComplete == false ->
      {get_evaluation_comparison(Condition, Value), []};

    true -> {ConditionResult, []}
  end.


eval_conditions(_User, _ConfigSpecs, []) -> {false, ""};

eval_conditions(User, ConfigSpecs, [Condition | RemainingConditions]) ->
  AllExposures = [],
  {ConditionResult, EvaluationComplete, Value, _Exposures} =
    get_evaluation_value(User, ConfigSpecs, Condition),
  if
    EvaluationComplete == false ->
      ComparisonResult = get_evaluation_comparison(Condition, Value),
      if
        ComparisonResult -> {ComparisonResult, AllExposures};
        true -> eval_conditions(User, ConfigSpecs, RemainingConditions)
      end;

    true ->
      if
        ConditionResult -> {ConditionResult, AllExposures};
        true -> eval_conditions(User, ConfigSpecs, RemainingConditions)
      end
  end.


get_evaluation_comparison(Condition, Value) ->
  Operator = string:casefold(maps:get(<<"operator">>, Condition, "")),
  Target = maps:get(<<"targetValue">>, Condition, ""),
  compare(Value, Operator, Target).


compare(Value, Operator, Target) ->
  case Operator of
    <<"gt">> ->
      if
        Value == unknown -> false;
        true -> Value > Target
      end;

    <<"gte">> ->
      if
        Value == unknown -> false;
        true -> Value >= Target
      end;

    <<"lt">> ->
      if
        Value == unknown -> false;
        true -> Value < Target
      end;

    <<"lte">> ->
      if
        Value == unknown -> false;
        true -> Value =< Target
      end;

    <<"version_gt">> ->
      if
        Value == unknown -> false;
        true -> version_compare(Value, Target, fun (Result) -> Result > 0 end)
      end;

    <<"version_gte">> ->
      if
        Value == unknown -> false;
        true -> version_compare(Value, Target, fun (Result) -> Result >= 0 end)
      end;

    <<"version_lt">> ->
      if
        Value == unknown -> false;
        true -> version_compare(Value, Target, fun (Result) -> Result < 0 end)
      end;

    <<"version_lte">> ->
      if
        Value == unknown -> false;
        true -> version_compare(Value, Target, fun (Result) -> Result =< 0 end)
      end;

    <<"version_eq">> ->
      if
        Value == unknown -> false;
        true -> version_compare(Value, Target, fun (Result) -> Result == 0 end)
      end;

    <<"version_neq">> ->
      if
        Value == unknown -> false;
        true -> version_compare(Value, Target, fun (Result) -> Result /= 0 end)
      end;

    <<"any">> ->
      if
        Value == unknown -> false;

        true ->
          list_any(
            Value,
            Target,
            generic_compare(true, fun (A, B) -> A == B end)
          )
      end;

    <<"none">> ->
      if
        Value == unknown -> true;

        true ->
          (
            not
            list_any(
              Value,
              Target,
              generic_compare(true, fun (A, B) -> A =:= B end)
            )
          )
      end;

    <<"any_case_sensitive">> ->
      if
        Value == unknown -> false;

        true ->
          list_any(
            Value,
            Target,
            generic_compare(false, fun (A, B) -> A =:= B end)
          )
      end;

    <<"none_case_sensitive">> ->
      if
        Value == unknown -> true;

        true ->
          not
          list_any(
            Value,
            Target,
            generic_compare(false, fun (A, B) -> A =:= B end)
          )
      end;

    <<"str_starts_with_any">> ->
      if
        Value == unknown -> false;

        true ->
          list_any(
            Value,
            Target,
            generic_compare(
              true,
              fun
                (Str, Prefix) ->
                  Prefix ++ string:find(Str, Prefix, trailing) =:= Str
              end
            )
          )
      end;

    <<"str_ends_with_any">> ->
      if
        Value == unknown -> false;

        true ->
          list_any(
            Value,
            Target,
            generic_compare(
              true,
              fun
                (Str, Postfix) ->
                  string:find(Str, Postfix, trailing) ++ Postfix =:= Str
              end
            )
          )
      end;

    <<"str_contains_any">> ->
      if
        Value == unknown -> false;

        true ->
          list_any(
            Value,
            Target,
            generic_compare(
              false,
              fun (A, B) -> string:find(A, B) /= nomatch end
            )
          )
      end;

    <<"str_contains_none">> ->
      if
        Value == unknown -> true;

        true ->
          not
          list_any(
            Value,
            Target,
            generic_compare(
              false,
              fun (A, B) -> string:find(A, B) /= nomatch end
            )
          )
      end;

    <<"str_matches">> ->
      if
        Value == unknown -> false;

        true ->
          case re:run(Value, Target) of
            {match, _Captured} -> true;
            nomatch -> false
          end
      end;

    <<"eq">> -> (Value =:= Target);
    <<"neq">> -> not (Value =:= Target);
    <<"before">> -> get_number(Value) < get_number(Target);
    <<"after">> -> get_number(Value) > get_number(Target);

    <<"on">> ->
      {{ValueYear, ValueMonth, ValueDay}, _ValueTime} =
        calendar:system_time_to_universal_time(round(get_number(Value)), 1000),
      {{TargetYear, TargetMonth, TargetDay}, _TargetTime} =
        calendar:system_time_to_universal_time(round(get_number(Target)), 1000),
      (ValueYear == TargetYear)
      and
      (ValueMonth == TargetMonth)
      and
      (ValueDay == TargetDay);

    _ ->
      erlang:display("UNSUPPORTED OPERATOR"),
      erlang:display(operator),
      false
  end.


generic_compare(IgnoreCase, Comparison) ->
  fun
    (A, B) ->
      if
        not (is_binary(A) and is_binary(B)) -> number_compare(A, B, Comparison);
        true -> string_compare(A, B, Comparison, IgnoreCase)
      end
  end.


string_compare(A, B, Comparison, IgnoreCase) ->
  StrA = binary_to_list(A),
  StrB = binary_to_list(B),
  if
    IgnoreCase -> Comparison(string:casefold(StrA), string:casefold(StrB));
    true -> Comparison(StrA, StrB)
  end.


number_compare(A, B, Comparison) ->
  NumA = get_number(A),
  NumB = get_number(B),
  Comparison(NumA, NumB).


get_number(Val) ->
  if
    is_integer(Val) -> Val * 1.0;
    is_binary(Val) -> list_to_num(binary_to_list(Val));
    is_list(Val) -> list_to_num(Val);
    is_float(Val) -> Val;
    true -> Val
  end.


list_to_num(N) ->
  case string:to_float(N) of
    {error, no_float} -> float(list_to_integer(N));
    {F, _Rest} -> float(F)
  end.


list_any(_Value, [], _Comparison) -> false;

list_any(Value, [El | TargetList], Comparison) ->
  Match = Comparison(Value, El),
  if
    Match == true -> true;
    true -> list_any(Value, TargetList, Comparison)
  end.


version_compare(Value, Target, Comparison) ->
  Version1 = trim(Value),
  Version2 = trim(Target),
  if
    length(Version1) == 0 -> false;
    length(Version2) == 0 -> false;
    true -> Comparison(version_compare_helper(Version1, Version2))
  end.


pad_list(0) -> [];
pad_list(N) when N > 0 -> ["0" | pad_list(N - 1)].

version_compare_helper(Version1, Version2) ->
  Parts1 = string:tokens(Version1, "."),
  Parts2 = string:tokens(Version2, "."),
  if
    length(Parts1) > length(Parts2) ->
      compare_version_part(
        Parts1,
        lists:append(Parts2, pad_list(length(Parts1) - length(Parts2)))
      );

    length(Parts2) > length(Parts1) ->
      compare_version_part(
        lists:append(Parts1, pad_list(length(Parts2) - length(Parts1))),
        Parts2
      );

    true -> compare_version_part(Parts1, Parts2)
  end.


compare_version_part([], []) -> 0;

compare_version_part([V | Rest], [V2 | Rest2]) ->
  if
    V > V2 -> 1;
    V < V2 -> -1;
    true -> compare_version_part(Rest, Rest2)
  end.


trim(Val) ->
  Str = binary_to_list(Val),
  Rest = string:find(Str, "-"),
  if
    Rest == nomatch -> Str;

    length(Rest) /= length(Str) ->
      string:slice(Str, 0, length(Str) - length(Rest));

    true -> Str
  end.


get_evaluation_value(User, ConfigSpecs, Condition) ->
  Type = maps:get(<<"type">>, Condition, ""),
  Target = maps:get(<<"targetValue">>, Condition, ""),
  Field = maps:get(<<"field">>, Condition, ""),
  IdType = maps:get(<<"idType">>, Condition, "userID"),
  case Type of
    <<"public">> -> {true, true, unknown, []};

    <<"pass_gate">> ->
      {_, Result, _JsonResult, _NestedRuleID, _NestedExposures} =
        eval_gate(User, ConfigSpecs, Target),
      % TODO secondary exposures
      {Result, true, unknown, []};

    <<"fail_gate">> ->
      {_, Result, _JsonResult, _NestedRuleID, _NestedExposures} =
        eval_gate(User, ConfigSpecs, Target),
      % TODO secondary exposures
      {not Result, true, unknown, []};

    <<"user_field">> ->
      Val = get_from_user(User, Field),
      {false, false, Val, []};

    <<"environment_field">> ->
      {false, false, get_from_environment(User, Field), []};

    <<"current_time">> -> {false, false, utils:get_timestamp(), []};

    <<"user_bucket">> ->
      AdditionValues = maps:get(<<"additionalValues">>, Condition, #{}),
      Salt = binary_to_list(maps:get(<<"salt">>, AdditionValues, <<"">>)),
      UnitID = binary_to_list(get_unit_id(User, IdType)),
      UserHash = compute_user_hash(Salt ++ "." ++ UnitID),
      {false, false, UserHash rem 1000, []};

    <<"unit_id">> -> {false, false, get_unit_id(User, IdType), []};
    % TODO ip_based, ua_based, current_time
    _ ->
      erlang:display("UNSUPPORTED TYPE"),
      erlang:display(Type),
      {false, true, unknown, []}
  end.


get_from_environment(User, Field) ->
  Environment = maps:get(<<"statsigEnvironment">>, User, #{}),
  get_or_lower(Field, Environment).


get_from_user(User, Field) ->
  Value = get_or_lower(Field, User),
  if
    Value == unknown ->
      Custom = maps:get(<<"custom">>, User, unknown),
      CustomValue = get_or_lower(Field, Custom),
      if
        CustomValue == unknown ->
          Private = maps:get(<<"privateAttributes">>, User, unknown),
          get_or_lower(Field, Private);

        true -> CustomValue
      end;

    true -> Value
  end.


get_or_lower(Field, Map) ->
  if
    Map == unknown -> unknown;

    true ->
      Value = maps:get(Field, Map, unknown),
      if
        Value == unknown ->
          LowerField = string:casefold(Field),
          maps:get(LowerField, Map, unknown);

        true -> Value
      end
  end.


get_unit_id(User, IdType) ->
  LowerId = string:casefold(IdType),
  if
    LowerId /= <<"userid">> ->
      Custom = get_or_lower(IdType, maps:get(<<"customIDs">>, User, unknown)),
      if
        Custom == unknown -> <<"">>;
        Custom == [] -> <<"">>;
        true -> Custom
      end;

    true ->
      UserID = maps:get(IdType, User, unknown),
      if
        UserID == null -> <<"">>;
        true -> UserID
      end
  end.


compute_user_hash(Value) ->
  Bits = crypto:hash(sha256, Value),
  <<Hash : 64, _Other/binary>> = Bits,
  Hash.


eval_pass_percent(User, Rule, ConfigSpec) ->
  ConfigSalt = binary_to_list(maps:get(<<"salt">>, ConfigSpec, "")),
  RuleSalt =
    binary_to_list(maps:get(<<"salt">>, Rule, maps:get(<<"id">>, Rule, ""))),
  IdType = maps:get(<<"idType">>, Rule, ""),
  UnitID = get_unit_id(User, IdType),
  Hash = compute_user_hash(ConfigSalt ++ "." ++ RuleSalt ++ "." ++ UnitID),
  PassPercent = maps:get(<<"passPercentage">>, Rule, 0),
  (Hash rem 10000) < (PassPercent * 100).
