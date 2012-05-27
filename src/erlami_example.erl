%%% Erlami examples.
%%%
%%% Copyright 2012 Marcelo Gornstein <marcelog@gmail.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
-module(erlami_example).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export([
    event_callback/2, listen_to/1, list_channels/1, list_commands/1,
    dial/4, dial/3
]).

dial(ServerName, Channel, {Context, Extension, Priority}) ->
    dial(ServerName, Channel, {Context, Extension, Priority}, []).

dial(ServerName, Channel, {Context, Extension, Priority}, Variables) ->
    Action = erlami_message:new_action(
        "Originate",
        [
            {"Channel", Channel}, {"Extension", Extension},
            {"Context", Context}, {"Priority", Priority}
        ],
        Variables
    ),
    erlami_client:send_action(ServerName, Action, fun response_callback/2).

list_channels(ServerName) ->
    Action = erlami_message:new_action("CoreShowChannels"),
    erlami_client:send_action(ServerName, Action, fun response_callback/2).

list_commands(ServerName) ->
    Action = erlami_message:new_action("ListCommands"),
    erlami_client:send_action(ServerName, Action, fun response_callback/2).

listen_to(ServerName) ->
    erlami_client:register_listener(
        ServerName, {fun event_callback/2, fun(Event) -> {ok, Value} = erlami_message:get(Event, "event"), Value =/= "DTMF" end}
    ).

event_callback(ServerName, Event) ->
    io:format("*****************~n"),
    io:format(
        "event from ~s ~p~n", [ServerName, erlami_message:to_list(Event)]
    ).

response_callback(Response, Events) ->
    io:format("*****************~n"),
    io:format("response: ~p~n", [erlami_message:to_list(Response)]),
    lists:foreach(
        fun(Event) ->
            io:format("Event: ~p", [erlami_message:to_list(Event)])
        end,
        Events
    ),
    io:format("*****************~n").
