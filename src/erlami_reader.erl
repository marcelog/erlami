%%% This module should be run as a linked process (see erlami_client),
%%% that spawn_link()s one of these through start_link/1. It will read lines
%%% coming in from asterisk until a complete message has been read. Then it
%%% will dispatch an event to the erlami_client to which is linked.
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
-module(erlami_reader).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").
-export([start_link/1]).

%%% @doc Starts an erlami_reader. The argument ErlamiClient is the name of
%%% a registered process, which is of type erlami_client. This will return the
%%% pid() of the newly created process.
-spec start_link(ErlamiClient::string()) -> pid().
start_link(ErlamiClient) ->
    spawn_link(
        fun() ->
            read_salutation(ErlamiClient),
            loop(ErlamiClient, [])
        end
    ).

%% @doc When an erlami_reader starts, it will read the first line coming in
%% from asterisk and notify the erlami_client, so it be calidated. After reading
%% the salutation, the main loop is entered.
-spec read_salutation(ErlamiClient::string()) -> none().
read_salutation(ErlamiClient) ->
    erlami_client:process_salutation(ErlamiClient, {salutation, wait_line()}),
    loop(ErlamiClient, []).

%% @doc The main loop will read lines coming in from asterisk until an empty
%% one is received, which should be the end of the message. The message is then
%% unmarshalled and dispatched.
-spec loop(ErlamiClient::string(), Acc::string()) -> none().
loop(ErlamiClient, Acc) ->
    NewAcc = case wait_line() of
        "\r\n" ->
            UnmarshalledMsg = erlami_message:unmarshall(Acc),
            dispatch_message(
                ErlamiClient, UnmarshalledMsg,
                erlami_message:is_response(UnmarshalledMsg),
                erlami_message:is_event(UnmarshalledMsg)
            ),
            [];
        Line -> string:concat(Acc, Line)
    end,
    loop(ErlamiClient, NewAcc).

%% @doc This function is used to select and dispatch a message by pattern
%% matching on its type. Will notify the erlami_client of a response or an
%% event.
-spec dispatch_message(
    ErlamiClient::string(), Message::erlami_message:message(),
    IsResponse::boolean(), IsEvent::boolean()
) -> none().
dispatch_message(ErlamiClient, Response, true, false) ->
    erlami_client:process_response(ErlamiClient, {response, Response});

dispatch_message(ErlamiClient, Event, false, true) ->
    erlami_client:process_event(ErlamiClient, {event, Event});

dispatch_message(_ErlamiClient, _Message, _IsResponse, _IsEvent) ->
    erlang:error(unknown_message).

%% @doc Reads a single line from asterisk.
-spec wait_line() -> string().
wait_line() ->
    receive
        {_Transport, _Socket, Line} -> Line;
        X ->
            error_logger:error_msg("Got: ~p", [X]),
            erlang:error(unknown_message)
    end.

