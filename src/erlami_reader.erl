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
-export([start_link/2]).
-include_lib("erlami_connection.hrl").

%% @doc Starts an erlami_reader. The argument ErlamiClient is the name of
%% a registered process, which is of type erlami_client. This will return the
%% pid() of the newly created process.
-spec start_link(
    ErlamiClient::string(), Connection::#erlami_connection{}
) -> pid().
start_link(ErlamiClient, #erlami_connection{}=Connection) ->
    spawn_link(
        fun() ->
            lager:debug("Waiting salutation"),
            read_salutation(ErlamiClient, Connection),
            lager:debug("Entering loop"),
            loop(ErlamiClient, Connection, [])
        end
    ).

%% @doc When an erlami_reader starts, it will read the first line coming in
%% from asterisk and notify the erlami_client, so it be calidated. After reading
%% the salutation, the main loop is entered.
-spec read_salutation(
    ErlamiClient::string(), Connection::#erlami_connection{}
) -> none().
read_salutation(ErlamiClient, Connection) ->
    Line = wait_line(Connection),
    lager:debug("Got as salutation: ~p", [Line]),
    erlami_client:process_salutation(ErlamiClient, {salutation, Line}).

%% @doc The main loop will read lines coming in from asterisk until an empty
%% one is received, which should be the end of the message. The message is then
%% unmarshalled and dispatched.
-spec loop(
    ErlamiClient::string(), Connection::#erlami_connection{},
    Acc::string()
) -> none().
loop(ErlamiClient, Connection, Acc) ->
    NewAcc = case wait_line(Connection) of
        "\r\n" ->
            UnmarshalledMsg = erlami_message:unmarshall(Acc),
            dispatch_message(
                ErlamiClient, UnmarshalledMsg,
                erlami_message:is_response(UnmarshalledMsg),
                erlami_message:is_event(UnmarshalledMsg),
                Acc
            ),
            [];
        Line -> string:concat(Acc, Line)
    end,
    loop(ErlamiClient, Connection, NewAcc).

%% @doc This function is used to select and dispatch a message by pattern
%% matching on its type. Will notify the erlami_client of a response or an
%% event.
-spec dispatch_message(
    ErlamiClient::string(), Message::erlami_message:message(),
    IsResponse::boolean(), IsEvent::boolean(), Original::string()
) -> none().
dispatch_message(ErlamiClient, Response, true, false, _Original) ->
    erlami_client:process_response(ErlamiClient, {response, Response});

dispatch_message(ErlamiClient, Event, false, true, _Original) ->
    erlami_client:process_event(ErlamiClient, {event, Event});

dispatch_message(_ErlamiClient, Message, _IsResponse, _IsEvent, Original) ->
    lager:error(
        "Unknown message: ~p -> ~p", [Original, erlami_message:to_list(Message)]
    ).
    %erlang:error(unknown_message).

%% @doc Reads a single line from asterisk.
-spec wait_line(Connection::#erlami_connection{}) -> string().
wait_line(#erlami_connection{read_line=Fun}=Connection) ->
    case Fun(10) of
        {ok, Line} -> Line;
        {error, timeout} ->
            receive
                {close} -> erlang:exit(shutdown)
            after 10 ->
                ok
            end,
            wait_line(Connection);
        {error, Reason} ->
            error_logger:error_msg("Got: ~p", [Reason]),
            erlang:error(Reason)
    end.
