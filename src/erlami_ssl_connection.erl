%%% Helper functions for connecting and sending data to the asterisk server
%%% (SSL).
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
-module(erlami_ssl_connection).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").
-define(SERVER, ?MODULE).

-behaviour(erlami_connection).

-include_lib("kernel/include/inet.hrl").
-include_lib("erlami_connection.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([open/1, read_line/2, send/2, close/1]).

%% @doc Will resolve and try to establish a connection to an asterisk box.
-spec open(Options::[{Key::atom(),Value::term()}]) -> {ssl:sslsocket()}.
open(Options) ->
    {host, Host} = lists:keyfind(host, 1, Options),
    {port, Port} = lists:keyfind(port, 1, Options),
    {ok, #hostent{h_addr_list=Addresses}}
        = erlami_connection:resolve_host(Host),
    {ok, Socket} = real_connect(Addresses, Port),
    {ok, #erlami_connection{
        send = fun(Data) ->
            ?MODULE:send(Socket, Data)
        end,
        read_line = fun(Timeout) ->
            ?MODULE:read_line(Socket, Timeout)
        end,
        close = fun() ->
            ?MODULE:close(Socket)
        end
    }}.

%% @doc Establishes a connection to the asterisk box, either via normal tcp or
%% tcp+ssl. Will try to get all available address for the given hostname or
%% ip address and try to connect to them in order. Will stop when a connection
%% can be established or when failed after trying each one of the addresses
%% found.
-spec real_connect(
    [Host::inet:hostname()], Port::inet:port_number()
) -> {ok, ssl:sslsocket()}.
real_connect([], _Port) ->
    outofaddresses;

real_connect([Address|Tail], Port) ->
    case ssl:connect(Address, Port, [{active, false}, {packet, line}]) of
        {ok, Socket} ->
            {ok, Socket};
        _ -> real_connect(Tail, Port)
    end.

%% @doc Used to send an action() via a tcp or tcp+ssl socket, selected by
%% pattern matching.
-spec send(Socket::ssl:sslsocket(), Action::erlami_message:action()) -> ok.
send(Socket = {sslsocket, new_ssl, _Port}, Action) ->
    ok = ssl:send(Socket, erlami_message:marshall(Action)).

%% @doc Closes and cleans up the connection.
-spec close(Socket::ssl:sslsocket()|gen_tcp:socket()) -> ok.
close({sslsocket, new_ssl, _Port}=Socket) ->
    ok = ssl:close(Socket).

%% @doc Used to get 1 line from AMI server.
-spec read_line(
    Socket::gen_tcp:socket(), Timeout::integer()
) -> {ok, Line::string()} | {error, Reason::term()}.
read_line(Socket, Timeout) ->
    % TO needed because of a bug in new_ssl? Without it, the connection
    % wont work (wont be able to read any data.. wtf?).
    ssl:recv(Socket, 0, 100).
