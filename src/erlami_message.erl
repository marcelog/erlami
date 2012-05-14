%%% This contains the types and logic needed to manage messages
%%% (i.e: actions, responses, and events), their attributes, and
%%% their variables. You should never try to do anything by
%%% yourself to a message, since this module should provide you
%%% with everything needed.
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
-module(erlami_message).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").
-define(EOL, "\r\n").
-define(EOM, ?EOL?EOL).

-include_lib("erlami_message.hrl").
%% ------------------------------------------------------------------
%% Types
%% ------------------------------------------------------------------
-type message() :: #erlami_message{}.
-type action() :: message().
-type response() :: message().
-type event() :: message().
-type attributes() :: dict().
-type variables() :: dict().
-type rawattribute() :: {Key::string(), Value::string()}.
-type rawattributes() :: [rawattribute()].
-type rawvariable() :: {Key::string(), Value::string()}.
-type rawvariables() :: [rawvariable()].

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    to_list/1, is_response/1, is_event/1,
    is_response_success/1, is_response_complete/1, is_event_last_for_response/1
]).

%% Functions for attribute handling.
-export([get/2, set/3, set_all/2]).

%% Functions for variable handling.
-export([get_variable/2, set_variable/3, set_all_variables/2]).

%% Marshalling/Unmarshalling.
-export([
    marshall/1, marshall/3, marshall_variable/2, marshall_variable/3,
    unmarshall/1
]).

%% Action handling.
-export([new_action/1, new_action/2, new_action/3]).

%% @doc Constructs a new message, doh.
-spec new_message() -> message().
new_message() ->
    new_message(dict:new(), dict:new()).

%% @doc Constructs a new message with the given attributes and variables.
-spec new_message(
    Attributes::attributes(), Variables::variables()
) -> #erlami_message{}.
new_message(Attributes, Variables) ->
    #erlami_message{attributes=Attributes, variables=Variables}.

%% @doc Constructs a new message of type action. It will add the action and
%% actionid attributes to it.
-spec new_action(Name::string()) -> action().
new_action(Name) ->
    {X1, X2, X3} = os:timestamp(),
    ActionId = string:join(
        [
            integer_to_list(X1),
            integer_to_list(X2),
            integer_to_list(X3)
        ], ""
    ),
    set_all(new_message(), [{"action", Name},{"actionid", ActionId}]).

%% @doc Constructs a new message of type action and sets the given attributes.
-spec new_action(Name::string(), Attributes::rawattributes()) -> action().
new_action(Name, Attributes) ->
    Action = new_action(Name),
    set_all(Action, Attributes).

%% @doc Constructs a new message of type action and sets the given attributes
%% and variables.
-spec new_action(
    Name::string(), Attributes::rawattributes(), Variables::rawvariables()
) -> action().
new_action(Name, Attributes, Variables) ->
    Action = new_action(Name),
    Action2 = set_all(Action, Attributes),
    set_all_variables(Action2, Variables).

%% @doc Fetches the value of the given message attribute.
-spec get(
    #erlami_message{attributes::attributes()}, Key::string()
) -> {ok, Value::string()} | notfound.
get(#erlami_message{attributes=Attributes}, Key) ->
    case dict:find(Key, Attributes) of
        {ok, Value} -> {ok, Value};
        _ -> notfound
    end.

%% @doc Fetches the value of the given message variable.
-spec get_variable(
    #erlami_message{variables::variables()}, Key::string()
) -> {ok, Value::string()} | notfound.
get_variable(#erlami_message{variables=Variables}, Key) ->
    case dict:find(Key, Variables) of
        {ok, Value} -> {ok, Value};
        _ -> notfound
    end.

%% @doc Returns a new message with the given attribute set.
-spec set(
    #erlami_message{attributes::attributes(), variables::variables()},
    Key::string(), Value::string()
) -> message().
set(
    #erlami_message{attributes=Attributes, variables=Variables}, Key, Value
) ->
    NewAttributes = dict:store(Key, Value, Attributes),
    new_message(NewAttributes, Variables).

%% @doc Returns a new message with all the given attributes added.
-spec set_all(
    Message::#erlami_message{}, Attributes::rawattributes()
) -> message().
set_all(#erlami_message{} = Message, Attributes) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            set(Acc, Key, Value)
        end,
        Message,
        Attributes
    ).

%% @doc Returns a new message with the given variable set.
-spec set_variable(
    #erlami_message{attributes::attributes(), variables::variables()},
    Key::string(), Value::string()
) -> message().
set_variable(
    #erlami_message{attributes=Attributes, variables=Variables},
    Key, Value
) ->
    NewVariables = dict:store(Key, Value, Variables),
    new_message(Attributes, NewVariables).

%% @doc Returns a new message with all the given variables added.
-spec set_all_variables(
    Message::#erlami_message{}, Variables::rawvariables()
) -> message().
set_all_variables(#erlami_message{} = Message, Variables) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            set_variable(Acc, Key, Value)
        end,
        Message,
        Variables
    ).

%% @doc Given a message, it will return the text representation of it, so it
%% can be sent by wire.
-spec marshall(
    #erlami_message{attributes::attributes(), variables::variables()}
) -> string().
marshall(#erlami_message{attributes=Attributes, variables=Variables}) ->
    MarshalledAtts = dict:fold(fun ?MODULE:marshall/3, "", Attributes),
    Msg = case dict:size(Variables) of
        0 -> MarshalledAtts;
        _ ->
            MarshalledVars = dict:fold(
                fun ?MODULE:marshall_variable/3, "", Variables
            ),
            lists:append(MarshalledAtts, MarshalledVars)
    end,
    string:concat(Msg, ?EOL).

%% @doc Given an attribute and its value, will return their text
%% representation, so it can be sent by wire.
-spec marshall(Key::string(), Value::string()) -> string().
marshall(Key, Value) ->
    string:join([Key, ": ", Value, ?EOL], "").

%% @doc When marshall()ing a message(), this will concatenate attribute line.
-spec marshall(Key::string(), Value::string(), Acc::string()) -> string().
marshall(Key, Value, Acc) ->
    string:concat(Acc, marshall(Key, Value)).

%% @doc Marshalls a variable.
-spec marshall_variable(Key::string(), Value::string()) -> string().
marshall_variable(Key, Value) ->
    marshall("Variable", string:join([Key, Value], "=")).

%% @doc Marshalls a variable with an accumulator (other marshalled variables).
-spec marshall_variable(
    Key::string(), Value::string(), Acc::string()
) -> string().
marshall_variable(Key, Value, Acc) ->
    string:concat(Acc, marshall_variable(Key, Value)).

%% @doc Given a text, this will split it into lines, returning a list of strings
%% (ehmm.. list of lists?).
-spec explode_lines(Text::string()) -> [string()].
explode_lines(Text) ->
    string:tokens(Text, ?EOL).

%% @doc Given a complete text message read from asterisk, this will transform
%% it to a message() so it is suitable to be operated on.
-spec unmarshall(Text::string()) -> message().
unmarshall(Text) ->
    Lines = explode_lines(Text),
    Message = new_message(),
    lists:foldl(
        fun(Line, Acc) ->
            Pos = string:str(Line, ":"),
            Key = string:to_lower(string:strip(
                string:substr(Line, 1, Pos - 1),
                both, 32
            )),
            Value = string:strip(string:substr(Line, Pos + 1), both, 32),
            case string:str(Key, "Variable") of
                0 -> erlami_message:set(Acc, Key, Value);
                _ ->
                    VarNamePos = string:str(Value, "="),
                    VarName = string:substr(Value, 1, VarNamePos - 1),
                    VarValue = string:substr(Value, VarNamePos + 1),
                    erlami_message:set_variable(Acc, VarName, VarValue)
            end
        end,
        Message,
        Lines
    ).

%% @doc Returns true if the given message is a response (i.e: it contains an
%% attribute "response".
-spec is_response(Message::message()) -> boolean().
is_response(Message) ->
    case get(Message, "response") of
        {ok, _Value} -> true;
        _ -> false
    end.

%% @doc Returns true if the given message is an event (i.e: it contains an
%% attribute "event".
-spec is_event(Message::message()) -> boolean().
is_event(Message) ->
    case get(Message, "event") of
        {ok, _Value} -> true;
        _ -> false
    end.

%% @doc Will return true if the message contains an attribute "response"
%% equals to "success".
-spec is_response_success(Response::message()) -> boolean().
is_response_success(Response) ->
    {ok, Value} = get(Response, "response"),
    string:equal(Value, "Success").

%% @doc Transforms the given message into a list of attributes and variables.
-spec to_list(
    #erlami_message{attributes::attributes(), variables::variables()}
) -> {rawattributes(), rawvariables()}.
to_list(#erlami_message{attributes=Attributes, variables=Variables}) ->
    {dict:to_list(Attributes), dict:to_list(Variables)}.

%% @doc Returns true if this response is complete or false if the response
%% needs to be completed with a stream of events.
-spec is_response_complete(Response::response()) -> boolean().
is_response_complete(Response) ->
    case erlami_message:get(Response, "message") of
        notfound -> true;
        {ok, ResponseText} ->
            case string:str(ResponseText, "ollow") of % "follows"
                0 -> true;
                _ -> false
            end
    end.

%% @doc Returns true if the given event is the one that completes the
%% event flow associated with a response, effectively completing the response.
-spec is_event_last_for_response(Event::event()) -> boolean().
is_event_last_for_response(Event) ->
    case erlami_message:get(Event, "eventlist") of
        notfound -> false;
        {ok, EventText} ->
            case string:str(EventText, "omplete") of
                0 -> false;
                _ -> true
            end
    end.
