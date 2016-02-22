-module(whatels_msg).

-export([encode/1,
         decode/1]).

-define(NL, <<"\r\n">>).

%% whatels msg:
%% OPNAME PayloadSize\r\n
%% Payload\r\n

%% == API

encode({symbols, Symbols}) ->
    encode_msg(<<"path-symbols">>, jsx:encode(Symbols));

encode({symbolsQ, Path}) ->
    encode_msg(<<"path-symbols?">>, Path);

encode({error, parse_error}) ->
    encode_msg(<<"error">>, <<"parse error">>).

decode(Bin) ->
    decode_msg(Bin, [], <<>>).

%% == Internal

encode_msg(BinOp, Payload) ->
    BinPayloadSize = integer_to_binary(byte_size(Payload)),
    <<BinOp/binary, " ", BinPayloadSize/binary, "\r\n",
      Payload/binary, "\r\n">>.

decode_msg(<<>>, MsgAcc, Remaining) ->
    F = fun({Op, Payload}) ->
        interp(Op, Payload)
    end,
    Msgs = lists:map(F, lists:reverse(MsgAcc)),
    {Msgs, Remaining};

decode_msg(Bin, MsgAcc, _) ->
    case decode_msg_flip(Bin) of
        remaining ->
            decode_msg(<<>>, MsgAcc, Bin);
        {Op, PayloadSize, NewBin} ->
            case decode_msg_flop(NewBin, PayloadSize) of
                remaining ->
                    decode_msg(<<>>, MsgAcc, Bin);
                {Payload, Rest} ->
                    Msg = {Op, Payload},
                    decode_msg(Rest, [Msg | MsgAcc], <<>>)
            end
    end.

decode_msg_flip(Bin) ->
    case binary:match(Bin, ?NL) of
        nomatch ->
            remaining;
        {Pos, 2} ->
            BinSize = byte_size(Bin),
            FlipBin = binary:part(Bin, {0, Pos}),
            {Op, PayloadSize} = extract_flip(FlipBin),
            Rest = binary:part(Bin, {Pos + 2, BinSize - (Pos + 2)}),
            {Op, PayloadSize, Rest}
    end.

decode_msg_flop(Bin, PayloadSize) ->
    BinSize = byte_size(Bin),
    case BinSize >= (PayloadSize + 2) of
        true ->
            Payload = binary:part(Bin, {0, PayloadSize}),
            Rest = binary:part(Bin, {PayloadSize + 2, BinSize - (PayloadSize + 2)}),
            {Payload, Rest};
        _ ->
            remaining
    end.

extract_flip(Bin) ->
    [Op, BinPayloadSize] = binary:split(Bin, <<" ">>),
    {Op, binary_to_integer(BinPayloadSize)}.

interp(<<"path-symbols">>, Payload) ->
    {symbols, jsx:decode(Payload, [return_maps])};

interp(<<"path-symbols?">>, Payload) ->
    {symbolsQ, Payload}.

extract_source(Payload) ->
    [Path, Source] = binary:split(Payload, <<"\r\n">>),
    {Path, Source}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    E = <<"path-symbols 43\r\n{\"functions\":[{\"line\":15,\"name\":\"getfun\"}]}\r\n">>,
    Symbols = #{<<"functions">> => [
        #{<<"name">> => <<"getfun">>,
          <<"line">> => 15}
    ]},
    R = whatels_msg:encode({symbols, Symbols}),
    ?assertEqual(E, R).

decode_test() ->
    E = {symbols, #{<<"functions">> => [
                    #{<<"name">> => <<"getfun">>,
                    <<"line">> => 15}]}},
    Bin = <<"path-symbols 43\r\n{\"functions\":[{\"line\":15,\"name\":\"getfun\"}]}\r\n">>,
    {[R], _} = whatels_msg:decode(Bin),
    ?assertEqual(E, R).

decode_symbols_test() ->
    Source = <<"apps/whatels/src/whatels_handler.erl">>,
    SourceSizeBin = integer_to_binary(byte_size(Source)),
    Bin = <<"path-symbols? ", SourceSizeBin/binary, "\r\n", Source/binary, "\r\n">>,
    E = {symbolsQ, Source},
    {[R], _} = whatels_msg:decode(Bin),
    ?assertEqual(E, R).

decode_symbols_leftovers_test() ->
    Bin = <<"path-symbols? 0\r\n">>,
    {Ls, Rem} = whatels_msg:decode(Bin),
    ?assertEqual(Ls, []),
    ?assertEqual(Rem, <<"path-symbols? 0\r\n">>).

-endif.