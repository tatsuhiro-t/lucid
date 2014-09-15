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
-module(writer_serv).
-behaviour(gen_server).

-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {socket, transport}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start(Args) ->
    gen_server:start(?MODULE, Args, []).

init([DispatcherPid, Socket, Transport]) ->
    erlang:monitor(process, DispatcherPid),
    {ok, #state{socket=Socket, transport=Transport}}.

handle_call({blob, Bin}, _From,
            State=#state{socket=Socket, transport=Transport}) ->
    case transport_send(Transport, Socket, Bin) of
        {error, _Reason} ->
            {stop, normal, State};
        ok ->
            {reply, ok, State}
    end.

handle_cast({blob, Bin}, State=#state{socket=Socket, transport=Transport}) ->
    case transport_send(Transport, Socket, Bin) of
        {error, _Reason} ->
            {stop, normal, State};
        ok ->
            {noreply, State}
    end.

handle_info({'DOWN', _Mon, process, _Pid, _Reason}, State) ->
    %% Dispatcher process down, exit requet_server as well
    {stop, normal, State};
handle_info(E, State) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    io:format("terminate reason: ~p~n", [Reason]).

transport_send(tcp, Socket, Bin) ->
    gen_tcp:send(Socket, Bin);
transport_send(ssl, Socket, Bin) ->
    ssl:send(Socket, Bin).
