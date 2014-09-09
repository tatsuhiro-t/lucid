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
-module(hpack).
-export([new_encoder/0, encode/2, encode_integer/3,
         new_decoder/0, decode/2, decode_integer/2]).
-export([test_int/0, test_encode/0, test_encdec/0, hexstr_to_bin/1]).
-include("header.hrl").
-include("hpack.hrl").

%% encoder

new_encoder() ->
    #hpackencoder{context=header:new_context(),
                  minsize=0,
                  contextupdate=false}.

encode(Headers, Encoder=#hpackencoder{contextupdate=ContextUpdate})
  when ContextUpdate =:= true ->
    {Buf, Encoder2} = context_update(Encoder),
    encode(Headers, Buf, Encoder2);
encode(Headers, Encoder) ->
    encode(Headers, <<>>, Encoder).

encode(Headers, IniBuf, Encoder) ->
    {ok,
     lists:foldl(fun(Header, {Acc, Enc}) ->
                         {Chunk, Enc2} = encode_header(Header, Enc),
                         {<<Acc/binary, Chunk/binary>>, Enc2}
                 end, {IniBuf, Encoder}, Headers)}.


context_update(Encoder=#hpackencoder{
                          minsize=MinSize,
                          context=#headercontext{maxsize=MaxSize}
                         }) when MinSize < MaxSize ->
    Buf = encode_table_size(MinSize),
    context_update(Buf, Encoder);
context_update(Encoder) ->
    context_update(<<>>, Encoder).

context_update(IniBuf, Encoder=#hpackencoder{
                                  context=#headercontext{maxsize=MaxSize}
                                 }) ->
    Buf = encode_table_size(MaxSize),
    {<<IniBuf/binary, Buf/binary>>, Encoder#hpackencoder{contextupdate=false}}.

encode_header({Name, Value}, Encoder=#hpackencoder{context=Context}) ->
    case header:search(Name, Value, Context) of
        {match, Index} ->
            Buf = encode_index(Index),
            {Buf, Encoder};
        {name_match, Index} ->
            {DoIndexing, Context2} = try_add_header(Name, Value, Context),
            Buf = encode_indname(Index, Value, DoIndexing),
            {Buf, Encoder#hpackencoder{context=Context2}};
        no_match ->
            {DoIndexing, Context2} = try_add_header(Name, Value, Context),
            Buf = encode_newname(Name, Value, DoIndexing),
            {Buf, Encoder#hpackencoder{context=Context2}}
    end.

try_add_header(Name, Value, Context) ->
    DoIndexing = header:should_index(Name, Value, Context),
    try_add_header(Name, Value, DoIndexing, Context).

try_add_header(Name, Value, true, Context) ->
    {ok, Context2} = header:add(Name, Value, Context),
    {true, Context2};
try_add_header(_Name, _Value, false, Context) ->
    {false, Context}.

encode_table_size(Size) ->
    encode_integer(Size, 5, 16#20).

encode_index(N) ->
    encode_integer(N, 7, 16#80).

encode_indname(N, Value, true) ->
    encode_indname(N, Value, 6, 16#40);
encode_indname(N, Value, false) ->
    encode_indname(N, Value, 4, 0).

encode_indname(N, Value, Prefix, FirstByte) ->
    Buf = encode_integer(N, Prefix, FirstByte),
    Buf2 = encode_string(Value),
    <<Buf/binary, Buf2/binary>>.

encode_newname(Name, Value, true) ->
    encode_newname2(Name, Value, 16#40);
encode_newname(Name, Value, false) ->
    encode_newname2(Name, Value, 0).

encode_newname2(Name, Value, FirstByte) ->
    Buf = <<FirstByte>>,
    Buf2 = encode_string(Name),
    Buf3 = encode_string(Value),
    <<Buf/binary, Buf2/binary, Buf3/binary>>.

encode_integer(N, Prefix, FirstByte) ->
    K = (1 bsl Prefix) - 1,
    encode_integer2(N, K, FirstByte).

encode_integer2(N, K, FirstByte) when N < K ->
    <<(N bor FirstByte)>>;
encode_integer2(N, K, FirstByte) ->
    M = N - K,
    encode_integer_acc(M, <<(K bor FirstByte)>>).

encode_integer_acc(N, Acc) when N < 128 ->
    <<Acc/binary, N>>;
encode_integer_acc(N, Acc) when N >= 128 ->
    Acc2 = <<Acc/binary, (16#80 bor (N band 16#7f))>>,
    encode_integer_acc(N bsr 7, Acc2).

encode_string(Bin) ->
    HuffmanLength = huffman:encode_length(Bin),
    encode_length(Bin, HuffmanLength).

encode_length(Bin, HuffmanLength) when HuffmanLength < size(Bin) ->
    Buf = encode_integer(HuffmanLength, 7, 16#80),
    Buf2 = huffman:encode(Bin),
    <<Buf/binary, Buf2/binary>>;
encode_length(Bin, _) ->
    Size = size(Bin),
    Buf = encode_integer(Size, 7, 0),
    <<Buf/binary, Bin/binary>>.

%% decoder

new_decoder() ->
    #hpackdecoder{context=#headercontext{}, maxsize=4096}.

decode(Bin, Decoder) ->
    decode(Bin, [], Decoder).

decode(Bin = <<2#1:1, _/bits>>, Acc, Decoder=#hpackdecoder{context=Context}) ->
    case decode_integer(Bin, 7) of
        {error, Why} ->
            {error, Why};
        {ok, {N, Rest}} ->
            case header:get(N, Context) of
                error ->
                    {error, outofindex};
                {Name, Value} ->
                    decode(Rest, [{Name, Value}|Acc], Decoder)
            end
    end;
%% Literal Header Field with incremental indexing - new name
decode(<<2#01000000:8, Bin/binary>>, Acc, Decoder) ->
    decode_newname(Bin, indexing, Acc, Decoder);
%% Literal Header Field with incremental indexing - index name
decode(Bin = <<2#01:2, _/bits>>, Acc, Decoder) ->
    decode_indname(Bin, indexing, Acc, Decoder);
%% Dynamic Table Size Update
decode(Bin = <<2#001:3, _/bits>>, Acc, Decoder) ->
    case decode_integer(Bin, 5) of
        {error, Why} ->
            {error, Why};
        {ok, {N, Rest}} ->
            decoder_change_table_size(N, Rest, Acc, Decoder)
    end;
decode(<<2#00010000:8, Bin/binary>>, Acc, Decoder) ->
    decode_newname(Bin, neverindexing, Acc, Decoder);
decode(Bin = <<2#0001:4, _/bits>>, Acc, Decoder) ->
    decode_indname(Bin, neverindexing, Acc, Decoder);
%% Literal Header Field without incremental indexing - new name
decode(<<2#00000000:8, Bin/binary>>, Acc, Decoder) ->
    decode_newname(Bin, withoutindexing, Acc, Decoder);
%% Literal Header Field without incremental indexing - index name
decode(Bin = <<2#0000:4, _/bits>>, Acc, Decoder) ->
    decode_indname(Bin, withoutindexing, Acc, Decoder);
decode(<<>>, Acc, Decoder) ->
    {ok, {lists:reverse(Acc), Decoder}}.

decoder_change_table_size(_, _, _, _) ->
    ok.

decode_indname(Bin, Mode, Acc, Decoder=#hpackdecoder{context=Context}) ->
    Prefix = case Mode of
                 indexing ->
                     6;
                 withoutindexing ->
                     4;
                 neverindexing ->
                     4
             end,
    case decode_integer(Bin, Prefix) of
        {error, Why} ->
            {error, Why};
        {ok, {Index, Rest}} ->
            case header:get(Index, Context) of
                error ->
                    {error, outofindex};
                {Name, _Value} ->
                    decode_value(Rest, Mode, Name, Acc, Decoder)
            end
    end.

decode_newname(Bin, Mode, Acc, Decoder) ->
    case decode_string(Bin) of
        {error, Why} ->
            {error, Why};
        {ok, {Name, Rest}} ->
            decode_value(Rest, Mode, Name, Acc, Decoder)
    end.

decode_value(Bin, Mode, Name, Acc, Decoder=#hpackdecoder{context=Context}) ->
    case decode_string(Bin) of
        {error, Why} ->
            {error, Why};
        {ok, {Value, Rest}} ->
            case Mode of
                indexing ->
                    {ok, Context2} = header:add(Name, Value, Context),
                    decode(Rest, [{Name, Value}|Acc],
                           Decoder#hpackdecoder{context=Context2});
                _ ->
                    decode(Rest, [{Name, Value}|Acc], Decoder)
            end
    end.

decode_integer(Bin, Prefix) ->
    K = (1 bsl Prefix) - 1,
    decode_integer2(Bin, K).

decode_integer2(<<N/integer, Rest/binary>>, K)
  when N band K =:= K ->
    decode_integer_acc(Rest, 0, K);
decode_integer2(<<N/integer, Rest/binary>>, K) ->
    {ok, {N band K, Rest}}.

decode_integer_acc(<<N/integer, Rest/binary>>, Shift, Acc) ->
    M = N band 16#7f,
    Acc2 = Acc + (M bsl Shift),
    case (N band 16#80) =:= 0 of
        true ->
            {ok, {Acc2, Rest}};
        false ->
            decode_integer_acc(Rest, Shift + 7, Acc2)
    end;
decode_integer_acc(<<>>, _Shift, _Acc) ->
    {error, eos}.

decode_string(Bin = <<2#1:1, _/bits>>) ->
    decode_string(Bin, huffman);
decode_string(Bin) ->
    decode_string(Bin, normal).

decode_string(Bin, Mode) ->
    case decode_integer(Bin, 7) of
        {error, Why} ->
            {error, Why};
        {ok, {Len, Rest}} when Len > size(Rest) ->
            {error, tooshortinput};
        {ok, {Len, Rest}} ->
            <<Value:Len/binary, Rest2/binary>> = Rest,
            decode_string_value(Value, Rest2, Mode)
    end.

decode_string_value(Bin, Rest, huffman) ->
    case huffman:decode(Bin) of
        {error, Why} ->
            {error, Why};
        {ok, Value} ->
            {ok, {Value, Rest}}
    end;
decode_string_value(Bin, Rest, normal) ->
    {ok, {Bin, Rest}}.

%% tests

test_int() ->
    Bin1337 = encode_integer(1337, 5, 0),
    {ok, {1337, <<>>}} = decode_integer(Bin1337, 5),
    Bin42 = encode_integer(42, 8, 0),
    {ok, {42, <<>>}} = decode_integer(Bin42, 8),
    BinInt64Max = encode_integer(1 bsl 64, 7, 128),
    {ok, {1 bsl 64, <<"garbage">>}} =
        decode_integer(<<BinInt64Max/binary, "garbage">>, 7).

test_encode() ->
    Encoder = new_encoder(),
    {ok, {Buf, _Encoder2}} =
        encode([{<<":path">>, <<"/">>},
                {<<":path">>, <<"/foo">>},
                {<<"digest">>, <<"sha-1=33333">>}], Encoder),
    Decoder = new_decoder(),
    {ok, {_Headers, _Decoder2}} = decode(Buf, Decoder).

%% http://necrobious.blogspot.jp/2008/03/binary-to-hex-string-back-to-binary-in.html
hexstr_to_bin(S) ->
    hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    hexstr_to_bin(T, [V | Acc]).

test_encdec() ->
    Input = ["488264026196dc34fd280654d27eea0801128166e32fdc036a62d1bf768586b19272ff0f1f919d29aee30c78f1e17abd24d4950b90f4b10f0d82101d408721eaa8a4498f57842507417f5f95497ca589d34d1f6a1271d882a60320eb3cf36fac1f",
             "88bfc1768dd06258741e54ad9326e61c5c1f4003703370a1bdae0fe74eac8a5fddad4bdab6a9af7427535eebe753570daa5de1b94d5bef7f3f4089f2b567f05b0b22d1fa868776b5f4e0df408cf2b0d15d454addcb620c7abf8712e05db03a277f0f0d01315885aec3771a4b5f92497ca58ae819aafb50938ec415305a99567b",
             "88c5c7c3c2c1c00f0d0131bfbe",
             "88c64087aaa21ca4498f57842507417f7b8b84842d695b05443c86aa6f6c96df697e940094d444a820044a099b8cb371b754c5a37f52848fd24a8f5a839bd9ab0f0d83081b6b5f86497ca582211f588faec3771a4bf4a523f2b0e62c02685f6496dc34fd280654d27eea0801128166e34cdc03ca62d1bf6196dc34fd280654d27eea0801128166e32fdc038a62d1bf7f0f88ea52d6b0e83772ff"],

    lists:foldl(fun(S, Dec) ->
                        Bin = hexstr_to_bin(S),
                        {ok, {Headers, Dec2}} = decode(Bin, Dec),
                        io:format("Headers: ~p~n", [Headers]),
                        Dec2
                end, new_decoder(), Input).
