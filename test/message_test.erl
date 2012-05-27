-module(message_test).
-include_lib("eunit/include/eunit.hrl").

start() ->
    ok.

stop(_SetupData) ->
    ok.

can_unmarshall(_SetupData) ->
    Msg = lists:concat([
        "action: Name\r\n",
        "actionid: 1\r\n",
        "att2: att2\r\n",
        "att1: att1\r\n",
        "\r\n"
    ]),
    Action = erlami_message:unmarshall(Msg),
    [?_assertEqual({ok, "Name"}, erlami_message:get(Action, "action")),
    ?_assertEqual({ok, "1"}, erlami_message:get(Action, "actionid")),
    ?_assertEqual({ok, "att1"}, erlami_message:get(Action, "att1")),
    ?_assertEqual({ok, "att2"}, erlami_message:get(Action, "att2"))].

can_marshall(_SetupData) ->
    Action = erlami_message:new_action(
        "Name",
        [{"att1", "att1"},{"att2", "att2"}],
        [{"var1", "var1"},{"var2", "var2"}]
    ),
    {ok, ActionId} = erlami_message:get(Action, "actionid"),
    ?_assertEqual(
        lists:concat([
            "action: Name\r\n",
            "actionid: ", ActionId, "\r\n",
            "att2: att2\r\n",
            "att1: att1\r\n",
            "Variable: var2=var2\r\n",
            "Variable: var1=var1\r\n",
            "\r\n"
        ]),
        erlami_message:marshall(Action)
    ).

can_return_notfound_on_unknown_attribute(_SetupData) ->
    Action = erlami_message:new_action("Name"),
    ?_assertEqual(notfound, erlami_message:get(Action, "whatever")).

can_return_notfound_on_unknown_variable(_SetupData) ->
    Action = erlami_message:new_action("Name"),
    ?_assertEqual(notfound, erlami_message:get_variable(Action, "whatever")).

can_build_action_with_name_atts_vars(_SetupData) ->
    Action = erlami_message:new_action(
        "Name",
        [{"att1", "att1"},{"att2", "att2"}],
        [{"var1", "var1"},{"var2", "var2"}]
    ),
    [?_assertEqual({ok, "Name"}, erlami_message:get(Action, "action")),
    ?_assertEqual({ok, "att1"}, erlami_message:get(Action, "att1")),
    ?_assertEqual({ok, "att2"}, erlami_message:get(Action, "att2")),
    ?_assertEqual({ok, "var1"}, erlami_message:get_variable(Action, "var1")),
    ?_assertEqual({ok, "var2"}, erlami_message:get_variable(Action, "var2"))].

can_build_action_with_name_atts(_SetupData) ->
    Action = erlami_message:new_action(
        "Name", [{"att1", "att1"},{"att2", "att2"}]
    ),
    [?_assertEqual({ok, "Name"}, erlami_message:get(Action, "action")),
    ?_assertEqual({ok, "att1"}, erlami_message:get(Action, "att1")),
    ?_assertEqual({ok, "att2"}, erlami_message:get(Action, "att2"))].

can_build_action_with_name(_SetupData) ->
    Action = erlami_message:new_action("Name"),
    ?_assertEqual({ok, "Name"}, erlami_message:get(Action, "action")).

message_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(SetupData) ->
            {inparallel, [
                can_build_action_with_name_atts_vars(SetupData),
                can_build_action_with_name_atts(SetupData),
                can_build_action_with_name(SetupData),
                can_return_notfound_on_unknown_attribute(SetupData),
                can_return_notfound_on_unknown_variable(SetupData),
                can_marshall(SetupData),
                can_unmarshall(SetupData)
            ]}
        end
    }.
