%% Lucid - HTTP/2 server in Erlang
%%
%% Copyright (c) 2014 Tatsuhiro Tsujikawa
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% This code is heavily based on the example code presented at
%% http://learnyousomeerlang.com/buckets-of-sockets.
-module(http2_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, SSLOptions} = application:get_env(ssl_options),
    {ok, SSLEnable} = application:get_env(ssl),
    TcpOptions = [binary, {reuseaddr, true}, {active, once}, {backlog, 1024}],
    {Transport, Options} =
        case SSLEnable of
            true ->
                {ssl, TcpOptions ++ SSLOptions};
            false ->
                {tcp, TcpOptions}
        end,
    {ok, ListenSocket} = transport_listen(Transport, Port, Options),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
          [{socket,
            {http2_serv, start_link, [ListenSocket, Transport]},
            temporary, 1000, worker, [http2_serv]}
          ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1, 20)],
    ok.

transport_listen(tcp, Port, Options) ->
    gen_tcp:listen(Port, Options);
transport_listen(ssl, Port, Options) ->
    ssl:listen(Port, Options).

