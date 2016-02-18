-module(whatels_e).

-export([ast/1,
         functions/1]).

ast(Path) when is_binary(Path) ->
    ast(binary_to_list(Path));

ast(Path) ->
    {ok, Ast} = epp:parse_file(Path, []),
    {ast, Ast}.

functions({ast, Ast}) ->
    F = fun(A, Acc) ->
        case A of
            {function, Line, Name, Arity, _} ->
                [{Name, Arity, Line} | Acc];
            _Other ->
                Acc
        end
    end,
    lists:reverse(lists:foldl(F, [], Ast)).


