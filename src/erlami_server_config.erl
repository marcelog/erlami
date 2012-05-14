%%% Helper functions to manipulate an asterisk server configuration.
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
-module(erlami_server_config).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    extract_host/1, extract_port/1, extract_transport/1,
    extract_username/1, extract_secret/1
]).

%% ------------------------------------------------------------------
%% Types
%% ------------------------------------------------------------------
-type transport() :: ssl | tcp.
-type username() :: string().
-type secret() :: string().
-type serverinfo() :: [serveroption()].
-type serveroption() :: {transport, transport()}
    | {host, inet:hostname()}
    | {port, inet:port_number()}
    | {username, username()}
    | {secret, secret()}
.

%% @doc Returns the hostname part of a server configuration.
-spec extract_host(ServerInfo::serverinfo()) -> string().
extract_host(ServerInfo) ->
    extract_key(host, ServerInfo).

%% @doc Returns the port part of a server configuration.
-spec extract_port(ServerInfo::serverinfo()) -> inet:port_number().
extract_port(ServerInfo) ->
    extract_key(port, ServerInfo).

%% @doc Returns the transport part of a server configuration.
-spec extract_transport(ServerInfo::serverinfo()) -> transport().
extract_transport(ServerInfo) ->
    extract_key(transport, ServerInfo).

%% @doc Returns the username part of a server configuration.
-spec extract_username(ServerInfo::serverinfo()) -> transport().
extract_username(ServerInfo) ->
    extract_key(username, ServerInfo).

%% @doc Returns the secret part of a server configuration.
-spec extract_secret(ServerInfo::serverinfo()) -> transport().
extract_secret(ServerInfo) ->
    extract_key(secret, ServerInfo).

%% @doc Returns a given key from a server configuration.
-spec extract_key(Key::atom(), ServerInfo::serverinfo()) -> string().
extract_key(Key, ServerInfo) ->
    {value, {Key, Value}} = lists:keysearch(Key, 1, ServerInfo),
    Value.