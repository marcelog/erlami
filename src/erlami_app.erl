%%% Application descriptor for the Erlami application.
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
-module(erlami_app).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    {ok, AsteriskServers} = application:get_env(servers),
    {ok, SupPid} = erlami_sup:start_link(),
    lists:foreach(
        fun({ServerName, ServerInfo}) ->
            WorkerName = erlami_client:get_worker_name(ServerName),
            lager:debug("Starting client supervisor: ~p", [WorkerName]),
            erlami_sup:start_child(ServerName, WorkerName, ServerInfo)
        end,
        AsteriskServers
    ),
    {ok, SupPid}.

stop(_State) ->
    ok.
