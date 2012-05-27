-module(reader_test).
-include_lib("eunit/include/eunit.hrl").

start() ->
    [{transport, ssl},
    {host, "127.0.0.1"},
    {port, 5039},
    {username, "username"},
    {secret, "secret"}].

stop(_SetupData) ->
    ok.

server_config_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(SetupData) ->
            {inparallel, [
            ]}
        end
    }.
