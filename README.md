statsig
=====

A feature flagging and experimentation library for erlang

Build
-----

    $ rebar3 compile

Run
-----
    $ rebar3 shell --apps statsig

% Usage:
%{ok, Pid} = gen_server:start(statsig, [{apiKey, ""}], []). 
% statsig:check_gate(Pid, #{<<"userID">> => <<"1234">>}, <<"test">>).
% statsig:check_gate(Pid, #{<<"userID">> => <<"12345">>}, <<"test">>).
% statsig:log_event(Pid, #{<<"userID">> => <<"12345">>}, <<"custom_event">>, 12, #{<<"test">> => <<"val">>}).
% statsig:flush(Pid).