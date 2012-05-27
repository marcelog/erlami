-module(server_config_test).
-include_lib("eunit/include/eunit.hrl").

start() ->
    [{connection, ssl},
    {host, "127.0.0.1"},
    {port, 5039},
    {username, "username"},
    {secret, "secret"}].

stop(_SetupData) ->
    ok.

can_get_host(SetupData) ->
    ?_assertEqual("127.0.0.1", erlami_server_config:extract_host(SetupData)).

can_get_port(SetupData) ->
    ?_assertEqual(5039, erlami_server_config:extract_port(SetupData)).

can_get_connection(SetupData) ->
    ?_assertEqual(ssl, erlami_server_config:extract_connection(SetupData)).

can_get_username(SetupData) ->
    ?_assertEqual("username", erlami_server_config:extract_username(SetupData)).

can_get_secret(SetupData) ->
    ?_assertEqual("secret", erlami_server_config:extract_secret(SetupData)).

server_config_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(SetupData) ->
            {inparallel, [
                can_get_host(SetupData),
                can_get_port(SetupData),
                can_get_connection(SetupData),
                can_get_username(SetupData),
                can_get_secret(SetupData)
            ]}
        end
    }.
