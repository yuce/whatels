-module(whatels_e).

-define(FOO, bar).

-export([ast_path/1,
         symbols/1]).

-spec ast_path(Path :: string() | binary()) ->
    {ok, list()}.

ast_path(Path) when is_binary(Path) ->
    ast_path(binary_to_list(Path));

ast_path(Path) ->
    {ok, Ast} = epp:parse_file(Path, []),
    {ast, Ast}.

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
