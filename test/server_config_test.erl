-module(server_config_test).
-include_lib("eunit/include/eunit.hrl").

start() ->
    [{transport, ssl},
    {host, "127.0.0.1"},
    {port, 5039},
    {username, "username"},
    {secret, "secret"}].

stop(_SetupData) ->
    ok.

can_get_host(SetupData) ->
    ?_assertEqual(erlami_server_config:extract_host(SetupData), "127.0.0.1").

can_get_port(SetupData) ->
    ?_assertEqual(erlami_server_config:extract_port(SetupData), 5039).

can_get_transport(SetupData) ->
    ?_assertEqual(erlami_server_config:extract_transport(SetupData), ssl).

can_get_username(SetupData) ->
    ?_assertEqual(erlami_server_config:extract_username(SetupData), "username").

can_get_secret(SetupData) ->
    ?_assertEqual(erlami_server_config:extract_secret(SetupData), "secret").

some_tricky_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(SetupData) ->
            {inparallel, [
                can_get_host(SetupData),
                can_get_port(SetupData),
                can_get_transport(SetupData),
                can_get_username(SetupData),
                can_get_secret(SetupData)
            ]}
        end
    }.
