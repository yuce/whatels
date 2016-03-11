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

-module(whatels_e).

-export([ast_path/1,
         symbols/1]).

-type module_name() :: 0 | atom().
%                         name     line      arity       exported?
-type function_info() :: {atom(), integer(), integer(), boolean()}.
-type error_info() :: {binary() | string(), integer()}.
-type symbols() :: #{functions => [function_info()],
                     errors => [error_info()],
                     module => module_name()}.


-spec ast_path(Path :: string() | binary()) ->
    {ast, list()} | {error, term()}.
ast_path(Path) when is_binary(Path) ->
    ast_path(binary_to_list(Path));

ast_path(Path) ->
    case epp:parse_file(Path, []) of
        {ok, Ast} ->
            {ast, Ast};
        {error, _} = Error ->
            Error
    end.

-spec symbols(A :: {ast, list()}) -> symbols().
symbols(Ast) ->
    {ModuleName, Functions, Errors, Exports} = extract_symbols(Ast),
    ExportsSet = sets:from_list(Exports),
    AnnotatedFunctions = annotate_functions(Functions, ExportsSet),
    #{functions => AnnotatedFunctions,
      errors => Errors,
      module => ModuleName}.

extract_symbols({ast, Ast}) ->
    F = fun(A, {Module, Functions, Errors, Exports} = Acc) ->
        case A of
            {function, Line, Name, Arity, _} ->
                {Module, [{Name, Arity, Line, false} | Functions], Errors, Exports};
            {error, {Line, _, _Err}} ->
                % TODO: exact error
                Err = <<"error">>,
                {Module, Functions, [{Err, Line} | Errors], Exports};
            {attribute, _, module, NewModule} when Module == 0 ->
                {NewModule, Functions, Errors, Exports};
            {attribute, _, export, NewExports} ->
                {Module, Functions, Errors, add_exports(NewExports, Exports)};
            _Other ->
                Acc
        end
    end,
    lists:foldl(F, {0, [], [], []}, Ast).

annotate_functions(Functions, ExportsSet) ->
    F = fun({Name, Arity, Line, _}) ->
        Exported = sets:is_element({Name, Arity}, ExportsSet),
        {Name, Arity, Line, Exported}
    end,
    lists:map(F, Functions).

add_exports([], Exports) ->
    Exports;
add_exports([H|T], Exports) ->
    add_exports(T, [H|Exports]).