-module(whatels_handler).
-behaviour(monkey_handler).

-export([init/1, handle/2]).

-define(WATCH_INTERVAL, 3000).

%% == Callbacks

init(_Args) ->
    {ok, Watch} = erwatch:new([{interval, ?WATCH_INTERVAL}]),
    State = #{base_url => undefined,
              remaining => <<>>,
              ast => #{},
              watch => Watch},
    {ok, State}.

handle({data, Data}, #{remaining := Rem} = State) ->
    NewData = <<Rem/binary, Data/binary>>,
    case decode(NewData) of
        parse_error ->
            ErrorMsg = whatels_msg:encode({error, parse_error}),
            {reply, ErrorMsg, State#{remaining := <<>>}};
        R ->
            {Msgs, NewRem} = R,
            State1 = State#{remaining := NewRem},
            case process_messages(Msgs, State1) of
                {[], NewState} ->
                    {noreply, NewState};
                {IoList, NewState} ->
                    {reply, IoList, NewState}
            end
    end;

handle({message, {erwatch@changes, _, Changes}}, State) ->
    lists:foreach(fun(C) -> process_change(C, State) end, Changes),
    {noreply, State};

handle({message, {symbolsQ, Path}}, State) ->
    Bin = path_bin_symbols(Path),
    {reply, Bin, State};

handle(_, State) ->
    {noreply, State}.

decode(Bin) ->
    try whatels_msg:decode(Bin) of
        R -> R
    catch
        error:Error ->
            io:format("ERROR: ~p~n", [Error]),
            parse_error
    end.

process_messages(Msgs, State) ->
    F = fun(M, {IoList, S}) ->
        case process_message(M, S) of
            {undefined, NewS} ->
                {IoList, NewS};
            {Bin, NewS} ->
                {[Bin | IoList], NewS}
        end
    end,
    {ProcMsgs, NewState} = lists:foldl(F, {[], State}, Msgs),
    {lists:reverse(ProcMsgs), NewState}.

-spec process_message(term(), map()) ->
    {undefined | binary(), map()}.

process_message({symbolsQ, Path}, State) ->
    Bin = path_bin_symbols(Path),
    {Bin, State};

process_message({watchX, BinWildcard}, #{watch := Watch} = State) ->
    Wildcard = binary_to_list(BinWildcard),
    erwatch:add_wildcard(Wildcard, Watch),
    {undefined, State}.

encode_functions(Functions) ->
    F = fun({Name, Arity, Line}) ->
        #{name => Name, arity => Arity, line => Line}
    end,
    lists:map(F, Functions).

encode_errors(Errors) ->
    F = fun({Err, Line}) ->
        #{error => Err, line => Line}
    end,
    lists:map(F, Errors).

encode_symbols(#{functions := Functions,
                 errors := Errors,
                 module := Module}) ->
    Base = #{functions => encode_functions(Functions),
             errors => encode_errors(Errors)},
    % If module name is given, use it.
    case Module of
        0 -> Base;
        _ -> Base#{module => Module}
    end.

process_change({deleted, _Path}, _State) ->
    ok;
process_change({_, Path}, _State) ->
    self() ! {symbolsQ, Path}.

path_bin_symbols(Path) ->
    Ast = whatels_e:ast_path(Path),
    Symbols = encode_symbols(whatels_e:symbols(Ast)),
    whatels_msg:encode({symbols, list_to_binary(Path), Symbols}).
