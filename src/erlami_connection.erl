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

-export([behaviour_info/1]).
-export([resolve_host/1]).

behaviour_info(callbacks) ->
    [{open,1}, {read_line,2}, {send,2}, {close,1}];

behaviour_info(_Other) ->
    undefined.

%% @doc Resolves a hostname or ip address into a hostent().
-spec resolve_host(Host::string()) -> {ok, inet:hostent()} | cantresolve.
resolve_host(Host) ->
    case inet:gethostbyaddr(Host) of
        {ok, Result} -> {ok, Result};
        _ -> inet:gethostbyname(Host)
    end.
