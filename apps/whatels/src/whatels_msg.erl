% Copyright (c) 2016, Yuce Tekol <yucetekol@gmail.com>.
% All rights reserved.

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:

% * Redistributions of source code must retain the above copyright
%   notice, this list of conditions and the following disclaimer.

% * Redistributions in binary form must reproduce the above copyright
%   notice, this list of conditions and the following disclaimer in the
%   documentation and/or other materials provided with the distribution.

% * The names of its contributors may not be used to endorse or promote
%   products derived from this software without specific prior written
%   permission.

% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(whatels_msg).

-export([encode/1,
         decode/1]).

%% whatels msg:
%% OPNAME PayloadSize\r\n
%% Payload\r\n

%% == API

encode({symbols, Path, Symbols}) when is_binary(Path) ->
    BinSymbols = jsx:encode(Symbols),
    Payload = <<Path/binary, "\r\n", BinSymbols/binary>>,
    yes_msg:encode(<<"path-symbols">>, Payload);

encode({symbolsQ, Path}) ->
    yes_msg:encode(<<"path-symbols?">>, Path);

encode({discard, Path}) ->
    yes_msg:encode(<<"discard!">>, Path);

encode({error, parse_error}) ->
    yes_msg:encode(<<"error">>, <<"parse error">>).

decode(Bin) ->
    decode_msg(Bin).

%% == Internal

decode_msg(Bin) ->
    F = fun({Op, Payload}) ->
        interp(Op, Payload)
    end,
    case yes_msg:decode(Bin) of
        {ok, BinMsgs, Rem} ->
            Msgs = lists:map(F, lists:reverse(BinMsgs)),
            {Msgs, Rem};
        {error, Error} ->
            {error, Error}
    end.

interp(<<"path-symbols">>, Payload) ->
    [Path, Json] = binary:split(Payload, <<"\r\n">>),
    {symbols, Path, jsx:decode(Json, [return_maps])};

interp(<<"path-symbols?">>, Payload) ->
    {symbolsQ, Payload};

interp(<<"watch!">>, Payload) ->
    {watchX, Payload}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    E = <<"path-symbols;53\r\n/tmp/foo\r\n{\"functions\":[{\"line\":15,\"name\":\"getfun\"}]}\r\n">>,
    Path = <<"/tmp/foo">>,
    Symbols = #{<<"functions">> => [
        #{<<"name">> => <<"getfun">>,
          <<"line">> => 15}
    ]},
    {ok, R} = whatels_msg:encode({symbols, Path, Symbols}),
    ?assertEqual(E, R).

decode_test() ->
    E = {symbols, <<"/tmp/foo">>, #{<<"functions">> => [
                    #{<<"name">> => <<"getfun">>,
                    <<"line">> => 15}]}},
    Bin = <<"path-symbols;53\r\n/tmp/foo\r\n{\"functions\":[{\"line\":15,\"name\":\"getfun\"}]}\r\n">>,
    {[R], _} = whatels_msg:decode(Bin),
    ?assertEqual(E, R).

decode_symbols_test() ->
    Source = <<"apps/whatels/src/whatels_handler.erl">>,
    SourceSizeBin = integer_to_binary(byte_size(Source)),
    Bin = <<"path-symbols?;", SourceSizeBin/binary, "\r\n", Source/binary, "\r\n">>,
    E = {symbolsQ, Source},
    {[R], _} = whatels_msg:decode(Bin),
    ?assertEqual(E, R).

decode_symbols_leftovers_test() ->
    Bin = <<"path-symbols?;0\r\n">>,
    {Ls, Rem} = whatels_msg:decode(Bin),
    ?assertEqual(Ls, []),
    ?assertEqual(Rem, <<"path-symbols?;0\r\n">>).

-endif.