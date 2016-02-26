-module(whatels_e).

-define(FOO, bar).

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
    {ast, list()}.

ast_path(Path) when is_binary(Path) ->
    ast_path(binary_to_list(Path));

ast_path(Path) ->
    {ok, Ast} = epp:parse_file(Path, []),
    {ast, Ast}.

-spec symbols(A :: {ast, list()}) -> symbols().
symbols(Ast) ->
    {ModuleName, Functions, Errors, Exports} = extract_symbols(Ast),
    ExportsSet = sets:from_list(Exports),
    io:format("exports set: ~p~n", [ExportsSet]),
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