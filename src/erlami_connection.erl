%%% Helper functions for connecting and sending data to the asterisk server.
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
-module(erlami_connection).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").
-define(SERVER, ?MODULE).

-include_lib("kernel/include/inet.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([connect/4, send/2, send/3]).

%% @doc Will resolve and try to establish a connection to an asterisk box.
-spec connect(
    Transport::erlami_server_config:transport(),
    Host::inet:hostname(), Port::inet:port_number(),
    Reader::pid()
) -> {ok, gen_tcp:socket()|ssl:sslsocket()}.
connect(Transport, Host, Port, Reader) ->
    {ok, #hostent{h_addr_list=Addresses}} = resolve_host(Host),
    real_connect(Transport, Addresses, Port, Reader).

%% @doc Establishes a connection to the asterisk box, either via normal tcp or
%% tcp+ssl. Will try to get all available address for the given hostname or
%% ip address and try to connect to them in order. Will stop when a connection
%% can be established or when failed after trying each one of the addresses
%% found.
-spec real_connect(
    Transport::erlami_server_config:transport(),
    [Host::inet:hostname()], Port::inet:port_number(),
    Reader::pid()
) -> {ok, gen_tcp:socket()|ssl:sslsocket()}.
real_connect(ssl, [], _Port, _Reader) ->
    outofaddresses;

real_connect(ssl, [Address|Tail], Port, Reader) ->
    case ssl:connect(Address, Port, [{packet, line}, {active, false}]) of
        {ok, Socket} ->
            ok = ssl:controlling_process(Socket, Reader),
            ok = ssl:setopts(Socket, [{active, true}]),
            {ok, Socket};
        _ -> real_connect(ssl, Tail, Port, Reader)
    end;

real_connect(tcp, [], _Port, _Reader) ->
    outofaddresses;

real_connect(tcp, [Address|Tail], Port, Reader) ->
    case gen_tcp:connect(Address, Port, [{packet, line}, {active, false}]) of
        {ok, Socket} ->
            ok = gen_tcp:controlling_process(Socket, Reader),
            ok = inet:setopts(Socket, [{active, true}]),
            {ok, Socket};
        _ -> real_connect(tcp, Tail, Port, Reader)
    end.

%% @doc Used to send an action() via a tcp or tcp+ssl socket, selected by
%% pattern matching.
-spec send(
    Socket::ssl:sslsocket()|gen_tcp:socket(), Action::erlami_message:action()
) -> ok.
send(Socket = {sslsocket, new_ssl, _Port}, Action) ->
    ok = ssl:send(Socket, erlami_message:marshall(Action));

send(Socket, Action) ->
    ok = gen_tcp:send(Socket, erlami_message:marshall(Action)).

%% @doc Used to create and send on demand a given action. Will create an
%% action() with the given name, add the requested attributes to it, and then
%% send it through the wire.
-spec send(
    Socket::ssl:sslsocket()|gen_tcp:socket(), ActionName::string(),
    Attributes::[{Key::string(), Value::string()}]
) -> ok.
send(Socket, ActionName, Attributes) ->
    Action1 = erlami_message:new_action(ActionName),
    Action2 = erlami_message:set_all(Action1, Attributes),
    ok = send(Socket, Action2).

%% @doc Resolves a hostname or ip address into a hostent().
-spec resolve_host(Host::string()) -> {ok, inet:hostent()} | cantresolve.
resolve_host(Host) ->
    case inet:gethostbyaddr(Host) of
        {ok, Result} -> {ok, Result};
        _ -> inet:gethostbyname(Host)
    end.

