statsig
=====

A feature flagging and experimentation library for erlang

Build
-----

    $ rebar3 compile

Run
-----
    $ rebar3 shell --apps statsig

## Usage:
```
application:set_env(statsig, statsig_api_key, ApiKey),
application:start(statsig),
statsig:check_gate(User, GateName),
statsig:log_event(#{<<"userID">> => <<"321">>}, <<"custom_event">>, 12, #{<<"test">> => <<"val">>}),
statsig:flush(Pid).
```