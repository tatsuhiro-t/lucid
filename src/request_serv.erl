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
-module(request_serv).
-behaviour(gen_server).

-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("http2.hrl").

-record(state, {dispatcher, %% PID of dispatcher process
                stream_id, %% Stream ID
                headers, %% Request headers
                file %% File if response is served from file
               }).

-define(CSS_FILENAME, "mystyle.css").

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start(Args) ->
    gen_server:start(?MODULE, Args, []).

init([DispatcherPid, StreamId, Headers]) ->
    erlang:monitor(process, DispatcherPid),
    gen_server:cast(self(), request),
    {ok, #state{dispatcher=DispatcherPid, stream_id=StreamId,
                headers=Headers}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(request, State) ->
    case handle_request(State) of
        stop ->
            {stop, normal, State};
        State2 ->
            {noreply, State2}
    end;
handle_cast(consumed, State) ->
    case resume_data(State) of
        stop ->
            {stop, normal, State};
        State2 ->
            {noreply, State2}
    end;
handle_cast(quit, State) ->
    {stop, normal, State}.

handle_info({'DOWN', _Mon, process, _Pid, _Reason}, State) ->
    %% Dispatcher process down, exit requet_server as well
    io:format("Dispatcher down\n"),
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

%%

reset_stream(ErrorCode, _State=#state{dispatcher=DispatcherPid,
                                      stream_id=StreamId}) ->
    gen_server:cast(DispatcherPid, {reset, StreamId, ErrorCode}),
    stop.

send_status_reply(StatusCode, _State=#state{dispatcher=DispatcherPid,
                                            stream_id=StreamId}) ->
    ResponseHeaders = [{<<":status">>, StatusCode},
                       {<<"server">>, <<"lucid">>},
                       {<<"content-type">>, <<"text/html; charset=utf-8">>}],
    Body = <<"<html><head></head><body>"
             "<h1>", StatusCode/binary, "</h1>"
             "</body></html>">>,
    gen_server:cast(DispatcherPid, {reply, StreamId, ResponseHeaders, Body,
                                    true}),
    stop.

resume_data(State=#state{dispatcher=DispatcherPid, stream_id=StreamId,
                         file=File}) ->
    {Body, Final} =
        case file:read(File, ?CHUNK_SIZE) of
            eof ->
                {<<>>, true};
            {ok, Chunk} ->
                {Chunk, false}
        end,
    gen_server:cast(DispatcherPid, {data, StreamId, Body, Final}),
    case Final of
        true ->
            stop;
        false ->
            State
    end.

handle_request(State=#state{dispatcher=DispatcherPid, stream_id=StreamId,
                            headers=Headers}) ->
    case get_mandatory_headers(Headers) of
        error ->
            reset_stream(?PROTOCOL_ERROR, State);
        {_M, _S, _A, <<"/"?CSS_FILENAME>>} ->
            ResponseHeaders =
                [{<<":status">>, <<"200">>},
                 {<<"server">>, <<"lucid">>},
                 {<<"content-type">>, <<"text/css; charset=utf-8">>}],
            Body = <<"h1 {color:red;text-align:center;}"
                     "h2 {color:red;}">>,
            gen_server:cast(DispatcherPid, {reply, StreamId, ResponseHeaders,
                                            Body, true}),
            stop;
        {_M, _S, _A, _P} ->
            ResponseHeaders =
                [{<<":status">>, <<"200">>},
                 {<<"server">>, <<"lucid">>},
                 {<<"content-type">>, <<"text/html; charset=utf-8">>}],
            Content = lists:foldl(fun({K, V}, Acc) ->
                                          <<Acc/binary, K/binary, ": ",
                                            V/binary, "\n">>
                                  end, <<>>, Headers),
            Body = <<"<html><head>"
                     "<link rel='stylesheet' type='text/css' href='"?CSS_FILENAME"'>"
                     "</head><body>"
                     "<h1>Hello Lucid/Erlang</h1>"
                     "<h2>Request Headers</h2><pre>",
                     (html_escape(Content))/binary,
                     "</pre>"
                     "</body></html>">>,

            PromisedHeaders =
                [{<<":method">>, <<"GET">>},
                 {<<":path">>, <<"/"?CSS_FILENAME>>},
                 {<<":scheme">>, <<"https">>}],
            gen_server:cast(DispatcherPid, {push, StreamId, PromisedHeaders}),

            gen_server:cast(DispatcherPid, {reply, StreamId, ResponseHeaders,
                                            Body, true}),
            stop
    end.

handle_request_file(State=#state{headers=Headers}) ->
    case get_mandatory_headers(Headers) of
        error ->
            reset_stream(?PROTOCOL_ERROR, State);
        {_, _, _, Path} ->
            case check_path(Path) of
                true ->
                    send_file(normalize_path(Path), State);
                false ->
                    send_status_reply(<<"403">>, State)
            end
    end.

send_file(Path, State=#state{dispatcher=DispatcherPid, stream_id=StreamId}) ->
    case file:open(<<".", Path/binary>>, [read, binary, raw]) of
        {error, _Reason} ->
            send_status_reply(<<"404">>, State);
        {ok, IoDevice} ->
            ResponseHeaders = [{<<":status">>, <<"200">>},
                               {<<"server">>, <<"lucid">>}],
            case file:read(IoDevice, ?CHUNK_SIZE) of
                eof ->
                    gen_server:cast(DispatcherPid, {reply, StreamId,
                                     ResponseHeaders, <<>>, true}),
                    stop;
                {ok, Body} ->
                    gen_server:cast(DispatcherPid, {reply, StreamId,
                                     ResponseHeaders, Body, false}),
                    State#state{file=IoDevice}
            end
    end.

normalize_path(Path) ->
    Len = size(Path),
    case binary:match(Path, <<"/">>, [{scope, {Len, -1}}]) of
        nomatch ->
            Path;
        _ ->
            <<Path/binary, "index.html">>
    end.

get_mandatory_headers(T) ->
    case get_mandatory_headers(T, {undefined, undefined, undefined,
                                   undefined, undefined}) of
        error ->
            error;
        {M, S, A, P, H} when A =:= undefined ->
            check_all_defined([M, S, H, P]);
        {M, S, A, P, _} ->
            check_all_defined([M, S, A, P])
    end.

check_all_defined(H = [M, S, A, P]) ->
    case lists:all(fun(X) -> X =/= undefined end, H) of
        true ->
            {M, S, A, P};
        false ->
            error
    end.

get_mandatory_headers([{<<":method">>, Method}|T],
                      {undefined, Scheme, Authority, Path, Host}) ->
    get_mandatory_headers(T, {Method, Scheme, Authority, Path, Host});
get_mandatory_headers([{<<":method">>, _}|_], _) ->
    error;
get_mandatory_headers([{<<":scheme">>, Scheme}|T],
                      {Method, undefined, Authority, Path, Host}) ->
    get_mandatory_headers(T, {Method, Scheme, Authority, Path, Host});
get_mandatory_headers([{<<":scheme">>, _}|_], _) ->
    error;
get_mandatory_headers([{<<":authority">>, Authority}|T],
                      {Method, Scheme, undefined, Path, Host}) ->
    get_mandatory_headers(T, {Method, Scheme, Authority, Path, Host});
get_mandatory_headers([{<<":authority">>, _}|_], _) ->
    error;
get_mandatory_headers([{<<":path">>, Path}|T],
                      {Method, Scheme, Authority, undefined, Host}) ->
    get_mandatory_headers(T, {Method, Scheme, Authority, Path, Host});
get_mandatory_headers([{<<":path">>, _}|_], _) ->
    error;
get_mandatory_headers([{<<"host">>, Host}|T],
                      {Method, Scheme, Authority, Path, undefined}) ->
    get_mandatory_headers(T, {Method, Scheme, Authority, Path, Host});
get_mandatory_headers([{<<"host">>, _}|_], _) ->
    error;
get_mandatory_headers([_|T], Acc) ->
    get_mandatory_headers(T, Acc);
get_mandatory_headers([], Acc) ->
    Acc.

check_path(Path) when size(Path) =:= 0 ->
    false;
check_path(Path) ->
    case binary:match(Path, <<"/">>, [{scope, {0, 1}}]) of
        nomatch ->
            false;
        _ ->
            case binary:match(Path, [<<"\\">>, <<"/../">>, <<"/./">>]) of
                nomatch ->
                    check_path_tail1(Path);
                _ ->
                    false
            end
    end.

check_path_tail1(Path) when size(Path) < 3 ->
    check_path_tail2(Path);
check_path_tail1(Path) ->
    Len = size(Path),
    case binary:match(Path, <<"/..">>, [{scope, {Len, -3}}]) of
        nomatch ->
            check_path_tail2(Path);
        _ ->
            false
    end.

check_path_tail2(Path) when size(Path) < 2 ->
    true;
check_path_tail2(Path) ->
    Len = size(Path),
    case binary:match(Path, <<"/.">>, [{scope, {Len, -2}}]) of
        nomatch ->
            true;
        _ ->
            false
    end.

html_escape(S0) ->
    S1 = re:replace(S0, <<"&">>, <<"\\&amp;">>, [{return, binary}, global]),
    S2 = re:replace(S1, <<"<">>, <<"\\&lt;">>, [{return, binary}, global]),
    S3 = re:replace(S2, <<">">>, <<"\\&gt;">>, [{return, binary}, global]),
    re:replace(S3, <<"'">>, <<"\\&#39;">>, [{return, binary}, global]).
