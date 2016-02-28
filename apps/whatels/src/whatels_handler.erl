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

-module(whatels_handler).
-behaviour(monkey_handler).

-export([init/1,
         handle/2]).

-define(WATCH_INTERVAL, 1000).

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

handle({message, {discard, _Path} = M}, State) ->
    Bin = whatels_msg:encode(M),
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
    F = fun({Name, Arity, Line, Exported}) ->
        #{name => Name,
          arity => Arity,
          line => Line,
          exported => Exported}
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

process_change({deleted, Path}, _State) ->
    self() ! {discard, Path};

process_change({_, Path}, _State) ->
    self() ! {symbolsQ, Path}.

path_bin_symbols(Path) ->
    Ast = whatels_e:ast_path(Path),
    Symbols = encode_symbols(whatels_e:symbols(Ast)),
    whatels_msg:encode({symbols, list_to_binary(Path), Symbols}).
