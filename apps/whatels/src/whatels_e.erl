-module(whatels_e).

-define(FOO, bar).

-export([ast_path/1,
         symbols/1]).

-type module_name() :: 0 | atom().
-type function_info() :: {atom(), integer(), integer()}.
-type error_info() :: {binary() | string(), integer()}.
-type symbols() :: #{functions => [function_info()],
                     errors => [error_info()],
                     module => module_name()}.


-spec ast_path(Path :: string() | binary()) ->
    {ast, list()}.

ast_path(Path) when is_binary(Path) ->
    ast_path(binary_to_list(Path));

ast_path(Path) ->
    {ok, Ast} = epp:parse_file(Path, []),
    {ast, Ast}.

-spec symbols(A :: {ast, list()}) -> symbols().
symbols({ast, Ast}) ->
    F = fun(A, {Module, Functions, Errors} = Acc) ->
        case A of
            {function, Line, Name, Arity, _} ->
                {Module, [{Name, Arity, Line} | Functions], Errors};
            {error, {Line, _, _Err}} ->
                % TODO: exact error
                Err = <<"error">>,
                {Module, Functions, [{Err, Line} | Errors]};
            {attribute, _, module, NewModule} when Module == 0 ->
                {NewModule, Functions, Errors};
            _Other ->
                Acc
        end
    end,
    {ModuleName, Functions, Errors} = lists:foldl(F, {0, [], []}, Ast),
    #{functions => lists:reverse(Functions),
      errors => lists:reverse(Errors),
      module => ModuleName}.
