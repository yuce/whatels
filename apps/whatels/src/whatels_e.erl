-module(whatels_e).

-define(FOO, bar).

-export([ast_path/1,
         symbols/1]).

ast_path(Path) when is_binary(Path) ->
    ast_path(binary_to_list(Path));

ast_path(Path) ->
    {ok, Ast} = epp:parse_file(Path, []),
    {ast, Ast}.

symbols({ast, Ast}) ->
    F = fun(A, {Functions, Errors} = Acc) ->
        case A of
            {function, Line, Name, Arity, _} ->
                {[{Name, Arity, Line} | Functions], Errors};
            {error, {Line, _, _Err}} ->
                % TODO: exact error
                Err = <<"error">>,
                {Functions, [{Err, Line} | Errors]};
            _Other ->
                Acc
        end
    end,
    {Functions, Errors} = lists:foldl(F, {[], []}, Ast),
    #{functions => lists:reverse(Functions),
      errors => lists:reverse(Errors)}.
