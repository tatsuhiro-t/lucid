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
-module(http2_serv).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("http2.hrl").

-define(MAX_WINDOW_SIZE, ((1 bsl 31) - 1)).
-define(MAX_CONCURRENT_STREAMS, 100).

-define(LOCAL_SESSION_WINDOW_SIZE, 65535).
-define(LOCAL_STREAM_WINDOW_SIZE, 65535).

-define(REMOTE_SESSION_WINDOW_SIZE, 65535).
-define(REMOTE_STREAM_WINDOW_SIZE, 65535).

-define(FRAME_HEADER_SIZE, 9).
-define(SETTINGS_ENTRY_SIZE, 6).

-define(MAX_HEADER_LIST_SIZE, 65536).

-define(DATA, 16#0).
-define(HEADERS, 16#1).
-define(RST_STREAM, 16#3).
-define(SETTINGS, 16#4).
-define(PING, 16#6).
-define(GOAWAY, 16#7).
-define(WINDOW_UPDATE, 16#8).
-define(CONTINUATION, 16#9).

-define(FLAG_NONE, 0). %% Defined for clarity
-define(FLAG_END_STREAM, 16#1).
-define(FLAG_ACK, 16#1).
-define(FLAG_END_HEADERS, 16#4).
-define(FLAG_PADDED, 16#8).
-define(FLAG_PRIORITY, 16#20).

-define(SETTINGS_MAX_HEADER_LIST_SIZE, 16#6).

-record(stream, {pid, %% PID of worker process
                 mon, %% monitor
                 stream_id, %% stream ID
                 remotewin=?REMOTE_STREAM_WINDOW_SIZE,
                 localwin=?LOCAL_STREAM_WINDOW_SIZE,
                 state=open %% stream state
		}).

-record(state, {socket, %% socket of this connection
                transport,
                writer,
                writermon,
                framer=expect_preface, %% framer state
                hpackenc=hpack:new_encoder(), %% hpack encoder
                hpackdec=hpack:new_decoder(), %% hpack decoder
                initial_window_size=?REMOTE_STREAM_WINDOW_SIZE,
                %% remove connection window size
                remotewin=?REMOTE_SESSION_WINDOW_SIZE,
                localwin=?LOCAL_SESSION_WINDOW_SIZE,
                outq=queue:new(),
                last_recv_stream_id=0,
                streams=[], %% orddict(), mapping stream ID to stream.
                pids=[], %% orddict(), mapping pid to stream ID.
                headersbuf,
                bin = <<>> %% buffer to store pending received data
               }).

start_link(ListenSocket, Transport) ->
    gen_server:start_link(?MODULE, [ListenSocket, Transport], []).

init([ListenSocket, Transport]) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket=ListenSocket, transport=Transport}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast({reply, StreamId, Headers, Body, Final}, State) ->
    case find_stream(StreamId, State) of
        error ->
            {noreply, State};
        {ok, Stream} ->
            case start_response(Stream, Headers, Body, Final, State) of
                stop ->
                    {stop, normal, State};
                State2 ->
                    {noreply, State2}
            end
    end;
handle_cast({data, StreamId, Body, Final},
            State=#state{remotewin=SessionWin}) ->
    case find_stream(StreamId, State) of
        error ->
            {noreply, State};
        {ok, Stream=#stream{remotewin=StreamWin}} ->
            AvailWin = min(SessionWin, StreamWin),
            case send_data(Stream, Body, Final, AvailWin, State) of
                stop ->
                    {stop, normal, State};
                State2 ->
                    {noreply, State2}
            end
    end;
handle_cast({reset, StreamId, ErrorCode}, State) ->
    State2 = transit_stream_state(reset, StreamId, State),
    send_rst_stream(StreamId, ErrorCode, State),
    {noreply, State2};
handle_cast(accept, State=#state{socket=ListenSocket, transport=Transport}) ->
    case transport_accept(Transport, ListenSocket) of
        {ok, Socket} ->
            http2_sup:start_socket(),
            ok = transport_handshake(Transport, Socket),
            ok = transport_setopts(Transport, Socket, [{active, once}]),
            {ok, WriterPid} = writer_serv:start([self(), Socket, Transport]),
            WriterMon = erlang:monitor(process, WriterPid),
            State2 = State#state{socket=Socket, writer=WriterPid,
                                 writermon=WriterMon},
            send_settings(State2),
            {noreply, State2};
        {error, _Reason} ->
            http2_sup:start_socket(),
            {noreply, State}
    end;
handle_cast(quit, State=#state{socket=Socket, transport=Transport}) ->
    transport_close(Transport, Socket),
    {stop, normal, State}.

handle_info({Tx, _Socket, Bin}, State=#state{bin=PrevBin})
  when Tx =:= tcp; Tx =:= ssl ->
    Data = <<PrevBin/binary, Bin/binary>>,
    case on_recv(Data, State#state{bin = <<>>}) of
        stop ->
            {stop, normal, State};
        State2 ->
            {noreply, State2}
    end;
handle_info({TxClosed, _Socket}, State)
  when TxClosed =:= tcp_closed; TxClosed =:= ssl_closed ->
    io:format("connection close~n"),
    {stop, normal, State};
handle_info({TxError, _Socket}, State)
  when TxError =:= tcp_error; TxError =:= ssl_error ->
    io:format("connection error~n"),
    {stop, normal, State};
handle_info({'DOWN', Mon, process, _Pid, _Reason},
            State=#state{writermon=Mon}) ->
    {stop, normal, State};
handle_info({'DOWN', _Mon, process, Pid, Reason}, State) ->
    %% One of request_serv process down
    io:format("request_serv down ~p:~p~n", [Pid, Reason]),
    case find_stream(Pid, State) of
        error ->
            {noreply, State};
        {ok, Stream=#stream{stream_id=StreamId, state=StreamState}} ->
            io:format("stream state ~p~n", [StreamState]),
            case StreamState of
                closed ->
                    {noreply, abandon_stream(Stream, State)};
                S when S =:= halfclosed_remote; S =:= open ->
                    send_rst_stream(StreamId, ?INTERNAL_ERROR, State),
                    {noreply, abandon_stream(Stream, State)};
                halfclosed_local ->
                    NewPids = orddict:erase(Pid, State#state.pids),
                    {noreply, State#state{pids=NewPids}}
            end
    end;
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

on_recv(<<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", Rest/binary>>,
        State=#state{framer=expect_preface}) ->
    on_recv(Rest, State#state{framer=read_frame, bin = <<>>});
on_recv(Bin, State=#state{framer=expect_preface}) when size(Bin) < 24 ->
    State#state{bin=Bin};
on_recv(_Bin, State=#state{framer=expect_preface}) ->
    terminate_now(State);
on_recv(<<Len:24, _Rest/binary>>, State) when Len > 16384 ->
    connection_error(State, ?PROTOCOL_ERROR);
on_recv(Bin = <<Len:24, _Type:8, _Flag:8, _:1, _StreamId:31, Rest/binary>>,
        State=#state{socket=Socket, transport=Transport})
  when Len > size(Rest) ->
    ok = transport_setopts(Transport, Socket, [{active, once}]),
    State#state{bin=Bin};
on_recv(<<Len:24, Type:8, Flag:8, _:1, StreamId:31, Rest/binary>>, State) ->
    io:format("Frame Len=~p Type=~p Flag=~2.16.0B StreamId=~p~n",
              [Len, Type, Flag, StreamId]),
    handle_frame(Rest, Len, Type, Flag, StreamId, State);
on_recv(Bin, State=#state{socket=Socket, transport=Transport})
  when size(Bin) < ?FRAME_HEADER_SIZE ->
    %% Sometimes we will call transport_setopts when underlying Socket
    %% was closed.
    case transport_setopts(Transport, Socket, [{active, once}]) of
        {error, _Reason} ->
            stop;
        ok ->
            State#state{bin=Bin}
    end.

handle_frame(_Bin, _Len, ?DATA, _Flag, 0, State) ->
    connection_error(State, ?PROTOCOL_ERROR);
handle_frame(_Bin, Len, ?DATA, _Flag, _StreamId,
             State=#state{localwin=SessionWin})
  when Len > SessionWin ->
    connection_error(State, ?FLOW_CONTROL_ERROR);
handle_frame(Bin, Len, ?DATA, Flag, StreamId, State) ->
    case find_stream(StreamId, State) of
        error ->
            %% Just ignore this for now.
            <<_:Len/binary, NextBin/binary>> = Bin,
            on_recv(NextBin, update_local_window(Len, State));
        {ok, Stream} ->
            {Payload, NextBin} = strip_padding(Bin, Len, Flag),
            process_upload_data(Len, Flag, Payload, NextBin, Stream, State)
    end;
handle_frame(_Bin, Len, ?CONTINUATION, _Flag, StreamId,
             State=#state{framer=expect_continuation,
                          headersbuf={HLen, _HFlag, StreamId, _HPayload}})
  when HLen + Len > ?MAX_HEADER_LIST_SIZE ->
    connection_error(State, ?PROTOCOL_ERROR);
handle_frame(Bin, Len, ?CONTINUATION, Flag, StreamId,
             State=#state{framer=expect_continuation,
                          headersbuf={HLen, HFlag, StreamId, HPayload}})
  when Flag band ?FLAG_END_HEADERS =:= 0 ->
    <<Payload:Len/binary, NextBin/binary>> = Bin,
    on_recv(NextBin,
            State#state{headersbuf={HLen + Len, HFlag, StreamId,
                                    <<HPayload/binary, Payload/binary>>}});
handle_frame(Bin, Len, ?CONTINUATION, _Flag, StreamId,
	     State=#state{framer=expect_continuation,
			  headersbuf={HLen, HFlag, StreamId, HPayload}}) ->
    handle_frame(<<HPayload/binary, Bin/binary>>, HLen + Len, ?HEADERS,
                 HFlag bor ?FLAG_END_HEADERS, StreamId,
                 State#state{framer=read_frame, headersbuf=undefined});
handle_frame(_Bin, _Len, ?CONTINUATION, _Flag, _StreamId, State) ->
    connection_error(State, ?PROTOCOL_ERROR);
handle_frame(_Bin, _Len, _Type, _Flag, _StreamId,
	     State=#state{framer=expect_continuation}) ->
    connection_error(State, ?PROTOCOL_ERROR);
handle_frame(Bin, Len, ?HEADERS, Flag, StreamId, State)
  when Flag band ?FLAG_END_HEADERS =:= 0 ->
    <<Payload:Len/binary, NextBin/binary>> = Bin,
    on_recv(NextBin,
            State#state{framer=expect_continuation,
                        headersbuf={Len, Flag, StreamId, Payload}});
handle_frame(Bin, Len, ?HEADERS, Flag, StreamId,
             State=#state{hpackdec=Decoder, streams=Streams})
  when StreamId rem 2 =:= 1 ->
    {Payload, NextBin} = strip_padding(Bin, Len, Flag),
    HeaderBlock  =
        case (Flag band ?FLAG_PRIORITY) > 0 of
            true ->
                <<_:1, _StreamDep:31, _Weidht:8, Rest/binary>> = Payload,
                Rest;
            false ->
                Payload
        end,
    case hpack:decode(HeaderBlock, Decoder) of
        {error, Why} ->
            io:format("Header decode failure ~p~n", [Why]),
            connection_error(State, ?COMPRESSION_ERROR);
        {ok, {Headers, Decoder2}} ->
            case find_stream(StreamId, State) of
                error ->
                    NumStreams = orddict:size(Streams),
                    create_new_stream(NextBin, StreamId, Flag, Headers,
                                      NumStreams,
                                      State#state{hpackdec=Decoder2});
                {ok, Stream} ->
                    handle_mid_stream_headers(NextBin, Flag, Stream,
                                              State#state{hpackdec=Decoder2})
            end
    end;
handle_frame(_Bin, _Len, ?HEADERS, _Flag, _StreamId, State) ->
    %% Even stream ID (e.g. 2, 4) is illegal
    connection_error(State, ?PROTOCOL_ERROR);
handle_frame(Bin, Len, ?SETTINGS, Flag, 0, State)
  when Len rem ?SETTINGS_ENTRY_SIZE =:= 0 ->
    <<Payload:Len/binary, NextBin/binary>> = Bin,
    case Flag band ?FLAG_ACK > 0 of
        true when Len =:= 0 ->
            on_recv(NextBin, State);
        true ->
            connection_error(State, ?PROTOCOL_ERROR);
        false ->
            State2 = parse_settings(Payload, State),
            on_recv(NextBin, State2)
    end;
handle_frame(_Bin, _, ?SETTINGS, _Flag, _, State) ->
    connection_error(State, ?FRAME_SIZE_ERROR);
handle_frame(<<_:1, Delta:31, NextBin/binary>>, 4, ?WINDOW_UPDATE, _Flag,
             0, State=#state{remotewin=SessionWin})
  when Delta > 0, Delta + SessionWin =< ?MAX_WINDOW_SIZE ->
    on_recv(NextBin, send_queue(State#state{remotewin=SessionWin + Delta}));
handle_frame(<<_:1, Delta:31, NextBin/binary>>, 4, ?WINDOW_UPDATE, _Flag,
             StreamId, State=#state{remotewin=SessionWin})
  when Delta > 0, Delta + SessionWin =< ?MAX_WINDOW_SIZE, StreamId =/= 0 ->
    case find_stream(StreamId, State) of
        error ->
            on_recv(NextBin, State);
        {ok, _Stream} ->
            Streams = orddict:update(StreamId,
                                     fun(S=#stream{remotewin=W}) ->
                                             S#stream{remotewin=W + Delta}
                                     end,
                                     State#state.streams),
            on_recv(NextBin, send_queue(State#state{streams=Streams}))
    end;
handle_frame(_Bin, _Len, ?WINDOW_UPDATE, _Flag, _StreamId, State) ->
    connection_error(State, ?PROTOCOL_ERROR);
handle_frame(<<_ErrorCode:32, NextBin/binary>>, 4, ?RST_STREAM, _Flag,
             StreamId, State) when StreamId =/= 0 ->
    State2 = transit_stream_state(reset, StreamId, State),
    on_recv(NextBin, State2);
handle_frame(_Bin, Len, ?RST_STREAM, _Flag, _StreamId, State)
  when Len =/= 4 ->
    connection_error(State, ?FRAME_SIZE_ERROR);
handle_frame(_Bin, _Len, ?RST_STREAM, _Flag, _StreamId, State) ->
    connection_error(State, ?PROTOCOL_ERROR);
handle_frame(<<Payload:64, NextBin/binary>>, 8, ?PING, Flag, 0, State) ->
    case Flag band ?FLAG_ACK > 0 of
        true ->
            ok;
        false ->
            send_ping(Payload, State)
    end,
    on_recv(NextBin, State);
handle_frame(_Bin, Len, ?PING, _Flag, _StreamId, State) when Len =/= 8 ->
    connection_error(State, ?FRAME_SIZE_ERROR);
handle_frame(_Bin, _Len, ?PING, _Flag, _StreamId, State) ->
    connection_error(State, ?PROTOCOL_ERROR);
handle_frame(<<_:1, LastStreamId:31, ErrorCode:32, Rest/binary>>, Len, ?GOAWAY,
             _Flag, 0, State) ->
    DebugDataLen = Len - 8,
    <<DebugData:DebugDataLen/binary, NextBin/binary>> = Rest,
    io:format("recv GOAWAY LastStreamId=~p, ErrorCode=~p, DebugData=~p~n",
              [LastStreamId, ErrorCode, DebugData]),
    on_recv(NextBin, State);
handle_frame(_Bin, Len, ?GOAWAY, _Flag, _StreamId, State)
  when Len < 8 ->
    connection_error(State, ?FRAME_SIZE_ERROR);
handle_frame(_Bin, _Len, ?GOAWAY, _Flag, _StreamId, State) ->
    connection_error(State, ?PROTOCOL_ERROR);
handle_frame(Bin, Len, Type, _Flag, _StreamId, State) ->
    io:format("unknown frame type ~p~n", [Type]),
    <<_Payload:Len/binary, NextBin/binary>> = Bin,
    on_recv(NextBin, State).

strip_padding(Bin, Len, Flag) when Flag band ?FLAG_PADDED =:= 0 ->
    <<Payload:Len/binary, Rest/binary>> = Bin,
    {Payload, Rest};
strip_padding(<<PadLen:8, Bin/binary>>, Len, _Flag) ->
    PayloadLen = Len - 1 - PadLen,
    <<Payload:PayloadLen/binary, _Pad:PadLen/binary, NextBin/binary>> = Bin,
    {Payload, NextBin}.

parse_settings(<<16#4:16, InitialWindowSize:32, Rest/binary>>,
               State=#state{streams=Streams,
                            initial_window_size=OldSize}) ->
    %% adjust remote window size for all existing streams.
    io:format("Change InitialWindowSize to ~p~n", [InitialWindowSize]),
    Streams2 = orddict:map(fun(_StreamId,
                               Stream=#stream{remotewin=Win}) ->
                                   NewWin = Win + InitialWindowSize - OldSize,
                                   Stream#stream{remotewin=NewWin}
                           end, Streams),
    parse_settings(Rest, State#state{initial_window_size=InitialWindowSize,
                                     streams=Streams2});
parse_settings(<<_:16, _:32, Rest/binary>>, State) ->
    parse_settings(Rest, State);
parse_settings(<<>>, State) ->
    send_settings_ack(State),
    State.

start_request(StreamId, Headers, StreamState, Streams, Pids) ->
    {ok, Pid} = request_serv:start([self(), StreamId, Headers]),
    Mon = erlang:monitor(process, Pid),
    Stream = #stream{pid=Pid, mon=Mon, stream_id=StreamId,
                     state=StreamState},
    Streams2 = orddict:store(StreamId, Stream, Streams),
    Pids2 = orddict:store(Pid, StreamId, Pids),
    {Streams2, Pids2}.

start_response(Stream=#stream{remotewin=StreamWin}, Headers, Body, Final,
               State=#state{remotewin=SessionWin}) ->
    State2 = send_headers(Stream, Headers, State),
    AvailWin = min(SessionWin, StreamWin),
    send_data(Stream, Body, Final, AvailWin, State2).

process_upload_data(Len, _Flag, _Payload, _NextBin,
                    _Stream=#stream{localwin=StreamWin}, State)
  when Len > StreamWin ->
    connection_error(State, ?FLOW_CONTROL_ERROR);
process_upload_data(Len, Flag, _Payload, NextBin, Stream, State) ->
    %% TODO send upload data to request_serv
    Final = Flag band ?FLAG_END_STREAM > 0,
    process_upload_data2(Final, Len, NextBin, Stream, State).

process_upload_data2(true, Len, NextBin,
                     _Stream=#stream{stream_id=StreamId}, State) ->
    State2 = transit_stream_state(end_stream_remote, StreamId, State),
    on_recv(NextBin, update_local_window(Len, State2));
process_upload_data2(false, Len, NextBin, Stream, State) ->
    on_recv(NextBin, update_local_window(Len, Stream, State)).

update_local_window(Len, _Stream=#stream{stream_id=StreamId,
                                         localwin=StreamWin},
                    State=#state{streams=Streams}) ->
    NewStreamWin = StreamWin - Len,
    NextWin = case NewStreamWin * 2 < ?LOCAL_STREAM_WINDOW_SIZE of
                  true ->
                      Delta = ?LOCAL_STREAM_WINDOW_SIZE - NewStreamWin,
                      send_window_update(StreamId, Delta, State),
                      ?LOCAL_STREAM_WINDOW_SIZE;
                  false ->
                      NewStreamWin
              end,
    Streams2 = orddict:update(StreamId,
                              fun(S) ->
                                      S#stream{localwin=NextWin}
                              end,
                              Streams),
    update_local_window(Len, State#state{streams=Streams2}).

update_local_window(Len, State=#state{localwin=SessionWin}) ->
    NewSessionWin = SessionWin - Len,
    NextWin = case NewSessionWin * 2 < ?LOCAL_SESSION_WINDOW_SIZE of
                  true ->
                      Delta = ?LOCAL_SESSION_WINDOW_SIZE - NewSessionWin,
                      send_window_update(0, Delta, State),
                      ?LOCAL_SESSION_WINDOW_SIZE;
                  false ->
                      NewSessionWin
              end,
    State#state{localwin=NextWin}.

send_headers(_Stream=#stream{stream_id=StreamId}, Headers,
             State=#state{hpackenc=Encoder}) ->
    {ok, {HeaderBlock, Encoder2}} = hpack:encode(Headers, Encoder),
    HeaderBlockSize = size(HeaderBlock),
    Bin = <<HeaderBlockSize:24, ?HEADERS:8, ?FLAG_END_HEADERS:8,
            0:1, StreamId:31, HeaderBlock/binary>>,
    ok = send(Bin, State),
    State#state{hpackenc=Encoder2}.

send_queue(State=#state{outq=Outq}) ->
    lists:foldl(fun({data, StreamId, Body, Final},
                    State2=#state{remotewin=SessionWin}) ->
                        case find_stream(StreamId, State2) of
                            error ->
                                State2;
                            {ok, Stream=#stream{remotewin=StreamWin}} ->
                                send_data(Stream, Body, Final,
                                          min(SessionWin, StreamWin), State2)
                        end
                end,
                State#state{outq=queue:new()},
                queue:to_list(Outq)).

send_data(_Stream=#stream{stream_id=StreamId}, Body, Final, AvailWin,
          State=#state{outq=Outq})
  when AvailWin =:= 0, size(Body) > 0 ->
    State#state{outq=queue:in({data, StreamId, Body, Final}, Outq)};
send_data(_Stream=#stream{stream_id=StreamId, remotewin=StreamWin, pid=Pid},
          Body, Final, AvailWin,
          State=#state{streams=Streams, remotewin=SessionWin}) ->
    BodySize = size(Body),
    Size = min(?CHUNK_SIZE, min(BodySize, AvailWin)),
    <<WrBody:Size/binary, Rest/binary>> = Body,
    {Event, Flag} = case Final of
                        true when size(Rest) =:= 0 ->
                            {end_stream_local, ?FLAG_END_STREAM};
                        _ ->
                            {none, 0}
                    end,
    Bin = <<Size:24, ?DATA:8, Flag:8, 0:1, StreamId:31, WrBody/binary>>,
    ok = send(Bin, State),
    case Final of
        false when size(Rest) =:= 0 ->
            %% Request request_serv to send more data
            gen_server:cast(Pid, consumed);
        _ ->
            ok
    end,
    Streams2 =
        orddict:update(StreamId,
                       fun(S) ->
                               S#stream{remotewin=StreamWin - Size}
                       end, Streams),
    State2 = transit_stream_state(Event, StreamId,
                                  State#state{remotewin=SessionWin - Size,
                                              streams=Streams2}),
    send_data_next(StreamId, Rest, Final, State2).

send_data_next(_StreamId, Rest, _Final, State)
  when size(Rest) =:= 0 ->
    State;
send_data_next(StreamId, Rest, Final, State=#state{remotewin=SessionWin}) ->
    case find_stream(StreamId, State) of
        error ->
            State;
        {ok, Stream=#stream{remotewin=StreamWin}} ->
            AvailWin = min(SessionWin, StreamWin),
            send_data(Stream, Rest, Final, AvailWin, State)
    end.

send_settings(State) ->
    Payload = <<?SETTINGS_MAX_HEADER_LIST_SIZE:16, ?MAX_HEADER_LIST_SIZE:32>>,
    Len = size(Payload),
    Bin = <<Len:24, ?SETTINGS:8, ?FLAG_NONE:8, 0:1, 0:31, Payload/binary>>,
    ok = send(Bin, State),
    ok.

send_settings_ack(State) ->
    Bin = <<0:24, ?SETTINGS:8, ?FLAG_ACK:8, 0:32>>,
    ok = send(Bin, State),
    ok.

send_rst_stream(StreamId, ErrorCode, State) ->
    Bin = <<4:24, ?RST_STREAM:8, ?FLAG_NONE:8, 0:1, StreamId:31, ErrorCode:32>>,
    ok = send(Bin, State),
    ok.

send_goaway(State=#state{last_recv_stream_id=LastRecvStreamId}, ErrorCode) ->
    Bin = <<8:24, ?GOAWAY:8, ?FLAG_NONE:8, 0:32, 0:1, LastRecvStreamId:31,
            ErrorCode:32>>,
    %% Sending GOAWAY is synchronous, since we are always send GOAWAY
    %% just before terminating connection.
    ok = send_sync(Bin, State),
    ok.

send_window_update(StreamId, Delta, State) ->
    Bin = <<4:24, ?WINDOW_UPDATE:8, ?FLAG_NONE:8, 0:1, StreamId:31,
            0:1, Delta:31>>,
    ok = send(Bin, State),
    ok.

send_ping(OpaqueData, State) ->
    Bin = <<8:24, ?PING:8, ?FLAG_ACK:8, 0:32, OpaqueData:64>>,
    ok = send(Bin, State),
    ok.

create_new_stream(NextBin, StreamId, _Flag, _Headers, _NumStreams,
                  State=#state{last_recv_stream_id=LastRecvStreamId})
  when StreamId =< LastRecvStreamId ->
    %% Ignore if StreamId =< LastRecvStreamId because after we remove
    %% stream, we may get HEADERS, which is valid.
    on_recv(NextBin, State);
create_new_stream(_NextBin, _StreamId, _Flag, _Headers, NumStreams, State)
  when NumStreams >= ? MAX_CONCURRENT_STREAMS ->
    connection_error(State, ?PROTOCOL_ERROR);
create_new_stream(NextBin, StreamId, Flag, Headers, _NumStreams,
                  State=#state{streams=Streams, pids=Pids}) ->
    %% New stream created
    io:format("Created Stream ID ~p~n", [StreamId]),
    StreamState =
        case Flag band ?FLAG_END_STREAM > 0 of
            true ->
                halfclosed_remote;
            false->
                open
        end,
    {Streams2, Pids2} = start_request(StreamId, Headers, StreamState,
                                      Streams, Pids),
    on_recv(NextBin, State#state{streams=Streams2, pids=Pids2,
                                 last_recv_stream_id=StreamId}).

handle_mid_stream_headers(_NextBin, Flag, _Stream=#stream{state=StreamState},
                          State)
  when StreamState =:= halfclosed_remote;
       StreamState =:= closed;
       StreamState =:= reserved_local;
       Flag band ?FLAG_END_STREAM =:= 0 ->
    connection_error(State, ?PROTOCOL_ERROR);
handle_mid_stream_headers(NextBin, _Flag,
                          _Stream=#stream{stream_id=StreamId}, State) ->
    State2 = transit_stream_state(end_stream_remote, StreamId, State),
    on_recv(NextBin, State2).

find_stream(Pid, State=#state{pids=Pids}) when is_pid(Pid) ->
    case orddict:find(Pid, Pids) of
        error ->
            error;
        {ok, StreamId} ->
            find_stream(StreamId, State)
    end;
find_stream(StreamId, _State=#state{streams=Streams})
  when is_integer(StreamId) ->
    orddict:find(StreamId, Streams).

transit_stream_state(none, _StreamId, State) ->
    State;
transit_stream_state(Event, StreamId, State=#state{streams=Streams}) ->
    case find_stream(StreamId, State) of
        error ->
            State;
        {ok, Stream} ->
            NextStreamState = next_stream_state(Event, Stream#stream.state),

            case NextStreamState of
                closed ->
                    abandon_stream(Stream, State);
                _ ->
                    Streams2 =
                        orddict:store(StreamId,
                                      Stream#stream{state=NextStreamState},
                                      Streams),
                    State#state{streams=Streams2}
            end
    end.

next_stream_state(end_stream_remote, open) ->
    halfclosed_remote;
next_stream_state(end_stream_remote, halfclosed_local) ->
    closed;
next_stream_state(end_stream_local, open) ->
    halfclosed_local;
next_stream_state(end_stream_local, halfclosed_remote) ->
    closed;
next_stream_state(reset, _StreamState) ->
    closed;
next_stream_state(_Event, StreamState) ->
    StreamState.

abandon_stream(_Stream=#stream{stream_id=StreamId, pid=Pid, mon=Mon},
               State=#state{streams=Streams, pids=Pids}) ->
    %% Demonitor here, so that we won't get DOWN message
    erlang:demonitor(Mon),
    NewPids = orddict:erase(Pid, Pids),
    gen_server:cast(Pid, quit),
    NewStreams = orddict:erase(StreamId, Streams),
    State#state{streams=NewStreams, pids=NewPids}.

connection_error(State, ErrorCode) ->
    send_goaway(State, ErrorCode),
    terminate_now(State).

terminate_now(_State) ->
    stop.

send(Bin, #state{writer=Pid}) ->
    gen_server:cast(Pid, {blob, Bin}),
    ok.

send_sync(Bin, #state{writer=Pid}) ->
    ok = gen_server:call(Pid, {blob, Bin}),
    ok.

transport_close(tcp, Socket) ->
    gen_tcp:close(Socket);
transport_close(ssl, Socket) ->
    ssl:close(Socket).

transport_accept(tcp, ListenSocket) ->
    gen_tcp:accept(ListenSocket);
transport_accept(ssl, ListenSocket) ->
    ssl:transport_accept(ListenSocket).

transport_handshake(tcp, _Socket) ->
    ok;
transport_handshake(ssl, Socket) ->
    case ssl:ssl_accept(Socket) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            {ok, <<"h2-14">>} = ssl:negotiated_next_protocol(Socket),
            ok
    end.

transport_setopts(tcp, Socket, Opts) ->
    ok = inet:setopts(Socket, Opts),
    ok;
transport_setopts(ssl, Socket, Opts) ->
    ssl:setopts(Socket, Opts).
