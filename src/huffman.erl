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
-module(huffman).
-export([encode/1, encode_length/1, decode/1, decode/2, test/0]).
-record(decode_context, {validity=accept, state=0, sofar}).

encode(<<>>) ->
    <<>>;
encode(<<T/bits>>) ->
    B = << << (huffmandata:symbol(<<C>>))/bits >> || <<C>> <= T >>,
    Padlen = 8 - (bit_size(B) rem 8),
    Pad = (1 bsl Padlen) - 1,
    <<B/bits, Pad:Padlen>>.

encode_length(T) ->
    lists:foldl(fun(C, Acc) ->
                        Acc + bit_size(huffmandata:symbol(<<C>>))
                end, 0, binary_to_list(T)).

decode(Bin) ->
    case decode(Bin, #decode_context{sofar = <<>>}) of
        {error, Why} ->
            {error, Why};
        #decode_context{validity=accept, sofar=Sofar} ->
            {ok, Sofar};
        _ ->
            {error, badhuffstr}
    end.

decode(<<>>, DecodeContext) ->
    DecodeContext;
decode(<<T/bits>>, DecodeContext=#decode_context{
                                    validity=Validity, state=State,
                                    sofar=Sofar}) ->
    case decode(T, Validity, State, Sofar) of
        {NewValidity, NewState, NewSoFar} ->
            DecodeContext#decode_context{validity=NewValidity,
                                         state=NewState,
                                         sofar=NewSoFar};
        {error, Why} ->
            {error, Why}
    end.

decode(<<>>, Validity, State, Acc) ->
    {Validity, State, Acc};
decode(<<H, T/bits>>, _Validity, State, Acc) ->
    case decode_half(H bsr 4, State, Acc) of
        {error, Why} ->
            {error, Why};
        {_, NextState, NextAcc} ->
            case decode_half(H band 15, NextState, NextAcc) of
                {error, Why} ->
                    {error, Why};
                {NextValidity, NextState2, NextAcc2} ->
                    decode(T, NextValidity, NextState2, NextAcc2)
            end
    end.

decode_half(I, State, Acc) ->
    case huffmandata:decode_state(State, I) of
        {fail, _, _} ->
            {error, huffman_bad_input};
        {NextAccept, NextState, no_symbol} ->
            {NextAccept, NextState, Acc};
        {NextAccept, NextState, Symbol} ->
            {NextAccept, NextState, <<Acc/bits, Symbol>>}
    end.

test() ->
    <<Enc1:5/binary, Enc2/binary>> =
        encode(<<"I have a bad feeling about this">>),
    Ctx = decode(Enc1, #decode_context{sofar = <<>>}),

    #decode_context{validity=Validty, state=State, sofar=Sofar} =
        decode(Enc2, Ctx),

    io:format("~p~n", [Ctx]),
    io:format("~p, ~p, ~p~n", [Validty, State, Sofar]).
