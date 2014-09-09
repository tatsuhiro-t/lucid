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
-module(header).
-export([new_context/0, get/2, add/3, search/3, should_index/3, test/0]).

-include("header.hrl").

-define(HEADER_OVERHEAD, 32).
-define(STATIC_TABLE_SIZE, 61).
-define(DEFAULT_TABLE_SIZE, 4096).

new_context() ->
    #headercontext{}.

new_context(Size) ->
    #headercontext{maxsize=Size}.

space({Name, Value}) ->
    size(Name) + size(Value) + ?HEADER_OVERHEAD.

add(Name, Value, Context) ->
    add(Name, Value, space({Name, Value}), Context).

add(_Name, _Value, Space, Context=#headercontext{maxsize=M}) when Space > M ->
    {ok, Context#headercontext{dyntable=[], size=0}};
add(Name, Value, Space, Context=#headercontext{maxsize=M}) when Space =< M ->
    Context2 = shrink(Space, Context),
    NewList = [{Name, Value} | Context2#headercontext.dyntable],
    NewSize = Context2#headercontext.size + Space,
    {ok, Context2#headercontext{dyntable=NewList, size=NewSize}}.

shrink(ExtraSpace, Context=#headercontext{dyntable=List, size=S, maxsize=M})
  when S + ExtraSpace > M ->

    RvList = lists:reverse(List),
    {NewRvList, NewSize} = popfront(ExtraSpace, RvList, S, M),
    Context#headercontext{dyntable=lists:reverse(NewRvList), size=NewSize};
shrink(_ExtraSpace, Context) ->
    Context.

popfront(ExtraSpace, [H|T], Size, MaxSize) ->
    Space = space(H),
    NewSize = Size - Space,
    case NewSize + ExtraSpace =< MaxSize of
	true ->
            {T, NewSize};
	false ->
            popfront(ExtraSpace, T, NewSize, MaxSize)
    end;
popfront(_ExtraSpace, [], 0, _MaxSize) ->
    {[], 0}.

search(Name, Value, _Context = #headercontext{dyntable=List}) ->

    case static_match(Name, Value) of
        {match, N} ->
            {match, N};
        {name_match, N} ->
            search(Name, Value, 1, N, List);
        nomatch ->
            search(Name, Value, 1, 0, List)
    end.

search(Name, Value, N, NameMatch, [H|T]) ->
    case H of
        {Name, Value} ->
            {match, N + ?STATIC_TABLE_SIZE};
        {Name, _} when NameMatch =:= 0 ->
            search(Name, Value, N + 1, N + ?STATIC_TABLE_SIZE, T);
        _ ->
            search(Name, Value, N + 1, NameMatch, T)
    end;
search(_, _, _, NameMatch, []) when NameMatch > 0 ->
    {name_match, NameMatch};
search(_, _, _, 0, []) ->
    nomatch.

should_index(Name, Value, _Context=#headercontext{maxsize=MaxSize}) ->
    space({Name, Value}) * 4 =< MaxSize * 3.

static_match(<<":authority">>, <<>>) ->
    {match, 1};
static_match(<<":authority">>, <<_/binary>>) ->
    {name_match, 1};
static_match(<<":method">>, <<"GET">>) ->
    {match, 2};
static_match(<<":method">>, <<_/binary>>) ->
    {name_match, 2};
static_match(<<":method">>, <<"POST">>) ->
    {match, 3};
static_match(<<":method">>, <<_/binary>>) ->
    {name_match, 3};
static_match(<<":path">>, <<"/">>) ->
    {match, 4};
static_match(<<":path">>, <<_/binary>>) ->
    {name_match, 4};
static_match(<<":path">>, <<"/index.html">>) ->
    {match, 5};
static_match(<<":path">>, <<_/binary>>) ->
    {name_match, 5};
static_match(<<":scheme">>, <<"http">>) ->
    {match, 6};
static_match(<<":scheme">>, <<_/binary>>) ->
    {name_match, 6};
static_match(<<":scheme">>, <<"https">>) ->
    {match, 7};
static_match(<<":scheme">>, <<_/binary>>) ->
    {name_match, 7};
static_match(<<":status">>, <<"200">>) ->
    {match, 8};
static_match(<<":status">>, <<_/binary>>) ->
    {name_match, 8};
static_match(<<":status">>, <<"204">>) ->
    {match, 9};
static_match(<<":status">>, <<_/binary>>) ->
    {name_match, 9};
static_match(<<":status">>, <<"206">>) ->
    {match, 10};
static_match(<<":status">>, <<_/binary>>) ->
    {name_match, 10};
static_match(<<":status">>, <<"304">>) ->
    {match, 11};
static_match(<<":status">>, <<_/binary>>) ->
    {name_match, 11};
static_match(<<":status">>, <<"400">>) ->
    {match, 12};
static_match(<<":status">>, <<_/binary>>) ->
    {name_match, 12};
static_match(<<":status">>, <<"404">>) ->
    {match, 13};
static_match(<<":status">>, <<_/binary>>) ->
    {name_match, 13};
static_match(<<":status">>, <<"500">>) ->
    {match, 14};
static_match(<<":status">>, <<_/binary>>) ->
    {name_match, 14};
static_match(<<"accept-charset">>, <<>>) ->
    {match, 15};
static_match(<<"accept-charset">>, <<_/binary>>) ->
    {name_match, 15};
static_match(<<"accept-encoding">>, <<"gzip, deflate">>) ->
    {match, 16};
static_match(<<"accept-encoding">>, <<_/binary>>) ->
    {name_match, 16};
static_match(<<"accept-language">>, <<>>) ->
    {match, 17};
static_match(<<"accept-language">>, <<_/binary>>) ->
    {name_match, 17};
static_match(<<"accept-ranges">>, <<>>) ->
    {match, 18};
static_match(<<"accept-ranges">>, <<_/binary>>) ->
    {name_match, 18};
static_match(<<"accept">>, <<>>) ->
    {match, 19};
static_match(<<"accept">>, <<_/binary>>) ->
    {name_match, 19};
static_match(<<"access-control-allow-origin">>, <<>>) ->
    {match, 20};
static_match(<<"access-control-allow-origin">>, <<_/binary>>) ->
    {name_match, 20};
static_match(<<"age">>, <<>>) ->
    {match, 21};
static_match(<<"age">>, <<_/binary>>) ->
    {name_match, 21};
static_match(<<"allow">>, <<>>) ->
    {match, 22};
static_match(<<"allow">>, <<_/binary>>) ->
    {name_match, 22};
static_match(<<"authorization">>, <<>>) ->
    {match, 23};
static_match(<<"authorization">>, <<_/binary>>) ->
    {name_match, 23};
static_match(<<"cache-control">>, <<>>) ->
    {match, 24};
static_match(<<"cache-control">>, <<_/binary>>) ->
    {name_match, 24};
static_match(<<"content-disposition">>, <<>>) ->
    {match, 25};
static_match(<<"content-disposition">>, <<_/binary>>) ->
    {name_match, 25};
static_match(<<"content-encoding">>, <<>>) ->
    {match, 26};
static_match(<<"content-encoding">>, <<_/binary>>) ->
    {name_match, 26};
static_match(<<"content-language">>, <<>>) ->
    {match, 27};
static_match(<<"content-language">>, <<_/binary>>) ->
    {name_match, 27};
static_match(<<"content-length">>, <<>>) ->
    {match, 28};
static_match(<<"content-length">>, <<_/binary>>) ->
    {name_match, 28};
static_match(<<"content-location">>, <<>>) ->
    {match, 29};
static_match(<<"content-location">>, <<_/binary>>) ->
    {name_match, 29};
static_match(<<"content-range">>, <<>>) ->
    {match, 30};
static_match(<<"content-range">>, <<_/binary>>) ->
    {name_match, 30};
static_match(<<"content-type">>, <<>>) ->
    {match, 31};
static_match(<<"content-type">>, <<_/binary>>) ->
    {name_match, 31};
static_match(<<"cookie">>, <<>>) ->
    {match, 32};
static_match(<<"cookie">>, <<_/binary>>) ->
    {name_match, 32};
static_match(<<"date">>, <<>>) ->
    {match, 33};
static_match(<<"date">>, <<_/binary>>) ->
    {name_match, 33};
static_match(<<"etag">>, <<>>) ->
    {match, 34};
static_match(<<"etag">>, <<_/binary>>) ->
    {name_match, 34};
static_match(<<"expect">>, <<>>) ->
    {match, 35};
static_match(<<"expect">>, <<_/binary>>) ->
    {name_match, 35};
static_match(<<"expires">>, <<>>) ->
    {match, 36};
static_match(<<"expires">>, <<_/binary>>) ->
    {name_match, 36};
static_match(<<"from">>, <<>>) ->
    {match, 37};
static_match(<<"from">>, <<_/binary>>) ->
    {name_match, 37};
static_match(<<"host">>, <<>>) ->
    {match, 38};
static_match(<<"host">>, <<_/binary>>) ->
    {name_match, 38};
static_match(<<"if-match">>, <<>>) ->
    {match, 39};
static_match(<<"if-match">>, <<_/binary>>) ->
    {name_match, 39};
static_match(<<"if-modified-since">>, <<>>) ->
    {match, 40};
static_match(<<"if-modified-since">>, <<_/binary>>) ->
    {name_match, 40};
static_match(<<"if-none-match">>, <<>>) ->
    {match, 41};
static_match(<<"if-none-match">>, <<_/binary>>) ->
    {name_match, 41};
static_match(<<"if-range">>, <<>>) ->
    {match, 42};
static_match(<<"if-range">>, <<_/binary>>) ->
    {name_match, 42};
static_match(<<"if-unmodified-since">>, <<>>) ->
    {match, 43};
static_match(<<"if-unmodified-since">>, <<_/binary>>) ->
    {name_match, 43};
static_match(<<"last-modified">>, <<>>) ->
    {match, 44};
static_match(<<"last-modified">>, <<_/binary>>) ->
    {name_match, 44};
static_match(<<"link">>, <<>>) ->
    {match, 45};
static_match(<<"link">>, <<_/binary>>) ->
    {name_match, 45};
static_match(<<"location">>, <<>>) ->
    {match, 46};
static_match(<<"location">>, <<_/binary>>) ->
    {name_match, 46};
static_match(<<"max-forwards">>, <<>>) ->
    {match, 47};
static_match(<<"max-forwards">>, <<_/binary>>) ->
    {name_match, 47};
static_match(<<"proxy-authenticate">>, <<>>) ->
    {match, 48};
static_match(<<"proxy-authenticate">>, <<_/binary>>) ->
    {name_match, 48};
static_match(<<"proxy-authorization">>, <<>>) ->
    {match, 49};
static_match(<<"proxy-authorization">>, <<_/binary>>) ->
    {name_match, 49};
static_match(<<"range">>, <<>>) ->
    {match, 50};
static_match(<<"range">>, <<_/binary>>) ->
    {name_match, 50};
static_match(<<"referer">>, <<>>) ->
    {match, 51};
static_match(<<"referer">>, <<_/binary>>) ->
    {name_match, 51};
static_match(<<"refresh">>, <<>>) ->
    {match, 52};
static_match(<<"refresh">>, <<_/binary>>) ->
    {name_match, 52};
static_match(<<"retry-after">>, <<>>) ->
    {match, 53};
static_match(<<"retry-after">>, <<_/binary>>) ->
    {name_match, 53};
static_match(<<"server">>, <<>>) ->
    {match, 54};
static_match(<<"server">>, <<_/binary>>) ->
    {name_match, 54};
static_match(<<"set-cookie">>, <<>>) ->
    {match, 55};
static_match(<<"set-cookie">>, <<_/binary>>) ->
    {name_match, 55};
static_match(<<"strict-transport-security">>, <<>>) ->
    {match, 56};
static_match(<<"strict-transport-security">>, <<_/binary>>) ->
    {name_match, 56};
static_match(<<"transfer-encoding">>, <<>>) ->
    {match, 57};
static_match(<<"transfer-encoding">>, <<_/binary>>) ->
    {name_match, 57};
static_match(<<"user-agent">>, <<>>) ->
    {match, 58};
static_match(<<"user-agent">>, <<_/binary>>) ->
    {name_match, 58};
static_match(<<"vary">>, <<>>) ->
    {match, 59};
static_match(<<"vary">>, <<_/binary>>) ->
    {name_match, 59};
static_match(<<"via">>, <<>>) ->
    {match, 60};
static_match(<<"via">>, <<_/binary>>) ->
    {name_match, 60};
static_match(<<"www-authenticate">>, <<>>) ->
    {match, 61};
static_match(<<"www-authenticate">>, <<_/binary>>) ->
    {name_match, 61};
static_match(<<_/binary>>, <<_/binary>>) ->
    nomatch.

get(1, _Context) ->
    {<<":authority">>, <<>>};
get(2, _Context) ->
    {<<":method">>, <<"GET">>};
get(3, _Context) ->
    {<<":method">>, <<"POST">>};
get(4, _Context) ->
    {<<":path">>, <<"/">>};
get(5, _Context) ->
    {<<":path">>, <<"/index.html">>};
get(6, _Context) ->
    {<<":scheme">>, <<"http">>};
get(7, _Context) ->
    {<<":scheme">>, <<"https">>};
get(8, _Context) ->
    {<<":status">>, <<"200">>};
get(9, _Context) ->
    {<<":status">>, <<"204">>};
get(10, _Context) ->
    {<<":status">>, <<"206">>};
get(11, _Context) ->
    {<<":status">>, <<"304">>};
get(12, _Context) ->
    {<<":status">>, <<"400">>};
get(13, _Context) ->
    {<<":status">>, <<"404">>};
get(14, _Context) ->
    {<<":status">>, <<"500">>};
get(15, _Context) ->
    {<<"accept-charset">>, <<>>};
get(16, _Context) ->
    {<<"accept-encoding">>, <<"gzip, deflate">>};
get(17, _Context) ->
    {<<"accept-language">>, <<>>};
get(18, _Context) ->
    {<<"accept-ranges">>, <<>>};
get(19, _Context) ->
    {<<"accept">>, <<>>};
get(20, _Context) ->
    {<<"access-control-allow-origin">>, <<>>};
get(21, _Context) ->
    {<<"age">>, <<>>};
get(22, _Context) ->
    {<<"allow">>, <<>>};
get(23, _Context) ->
    {<<"authorization">>, <<>>};
get(24, _Context) ->
    {<<"cache-control">>, <<>>};
get(25, _Context) ->
    {<<"content-disposition">>, <<>>};
get(26, _Context) ->
    {<<"content-encoding">>, <<>>};
get(27, _Context) ->
    {<<"content-language">>, <<>>};
get(28, _Context) ->
    {<<"content-length">>, <<>>};
get(29, _Context) ->
    {<<"content-location">>, <<>>};
get(30, _Context) ->
    {<<"content-range">>, <<>>};
get(31, _Context) ->
    {<<"content-type">>, <<>>};
get(32, _Context) ->
    {<<"cookie">>, <<>>};
get(33, _Context) ->
    {<<"date">>, <<>>};
get(34, _Context) ->
    {<<"etag">>, <<>>};
get(35, _Context) ->
    {<<"expect">>, <<>>};
get(36, _Context) ->
    {<<"expires">>, <<>>};
get(37, _Context) ->
    {<<"from">>, <<>>};
get(38, _Context) ->
    {<<"host">>, <<>>};
get(39, _Context) ->
    {<<"if-match">>, <<>>};
get(40, _Context) ->
    {<<"if-modified-since">>, <<>>};
get(41, _Context) ->
    {<<"if-none-match">>, <<>>};
get(42, _Context) ->
    {<<"if-range">>, <<>>};
get(43, _Context) ->
    {<<"if-unmodified-since">>, <<>>};
get(44, _Context) ->
    {<<"last-modified">>, <<>>};
get(45, _Context) ->
    {<<"link">>, <<>>};
get(46, _Context) ->
    {<<"location">>, <<>>};
get(47, _Context) ->
    {<<"max-forwards">>, <<>>};
get(48, _Context) ->
    {<<"proxy-authenticate">>, <<>>};
get(49, _Context) ->
    {<<"proxy-authorization">>, <<>>};
get(50, _Context) ->
    {<<"range">>, <<>>};
get(51, _Context) ->
    {<<"referer">>, <<>>};
get(52, _Context) ->
    {<<"refresh">>, <<>>};
get(53, _Context) ->
    {<<"retry-after">>, <<>>};
get(54, _Context) ->
    {<<"server">>, <<>>};
get(55, _Context) ->
    {<<"set-cookie">>, <<>>};
get(56, _Context) ->
    {<<"strict-transport-security">>, <<>>};
get(57, _Context) ->
    {<<"transfer-encoding">>, <<>>};
get(58, _Context) ->
    {<<"user-agent">>, <<>>};
get(59, _Context) ->
    {<<"vary">>, <<>>};
get(60, _Context) ->
    {<<"via">>, <<>>};
get(61, _Context) ->
    {<<"www-authenticate">>, <<>>};
get(N, _Context=#headercontext{dyntable=List})
  when N > ?STATIC_TABLE_SIZE, N =< ?STATIC_TABLE_SIZE + length(List) ->
    lists:nth(N - ?STATIC_TABLE_SIZE, List);
get(_, _) ->
    error.

test() ->
    Context = new_context(14 + ?HEADER_OVERHEAD),
    {match, 2} = search(<<":method">>, <<"GET">>, Context),
    {name_match, 4} = search(<<":path">>, <<"/foobar">>, Context),
    nomatch = search(<<"my-header">>, <<"myval">>, Context),
    {ok, Context2} = add(<<"my-header">>, <<"myval">>, Context),
    {match, 62} = search(<<"my-header">>, <<"myval">>, Context2),
    {ok, Context3} = add(<<"authorizaton">>, <<"ba">>, Context2),
    {match, 62} = search(<<"authorizaton">>, <<"ba">>, Context3),
    nomatch = search(<<"my-header">>, <<"myval">>, Context3).
