-module(prop_logic).

-include_lib("proper/include/proper.hrl").

-export([ initial_state/0
        , command/1
        , precondition/2
        , postcondition/3
        , next_state/3
        ]).

prop_logic() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                setup(),
                {H, S, Result} = run_commands(?MODULE, Cmds),
                teardown(),
                ?WHENFAIL(
                   io:format("History: ~w~nState: ~w~nRes: ~w~n",
                             [H, S, Result]),
                   aggregate(args_length(Cmds), Result =:= ok))
            end).

setup() ->
    application:ensure_all_started(dos).

teardown() ->
    ok.

initial_state() ->
    [].

command(_S) ->
    %% Basically generate tasks, and inject some
    %% dependencies between those tasks.
    ?LET(Tasks, tasks(),
         {call, dos_logic, order, [dependencies(Tasks)]}).

%% Lot of optimisations possible here to test relevant stuff
precondition(_S, {call, dos_logic, order, [Tasks]}) ->
    dos_logic:check_no_duplicates(Tasks);
precondition(_S, _C) ->
    true.

postcondition(_S, {call, dos_logic, order, [[]]}, {ok, []}) ->
    true;
postcondition(_S, {call, dos_logic, order, [Tasks]}, {ok, Res}) ->
    %% For each task in the results should exist in the task list
    InNames = [N || #{<<"name">> := N} <- Tasks],
    OutNames = [N || #{<<"name">> := N} <- Res],
    [] == InNames -- OutNames;
postcondition(_S, _C, {error, _}) ->
    true;
postcondition(_S, _C, _R) ->
    false.

next_state(S, _R, _C) ->
    S.

%%% Classifier

args_length(Cmds) ->
    lists:map(fun ({set, _Var, {call, _M, _F, [Args]}}) ->
                case dos_logic:order(Args) of
                    {error, _} = Err ->
                        Err;
                    _ ->
                        {arg_length, length(Args)}
                end
              end,
              Cmds).

%%% Generators

tasks() ->
    list(task()).

task() ->
    ?LET({N, C}, {name(), command()},
         elements(
           [ #{ <<"name">> => N
              , <<"command">> => C
              }
           ])).

name() ->
    string().
command() ->
    string().
dependencies(Ls) ->
    MaxDepsLength = 3,
    [frequency([ {40, generate_deps(L, Ls, MaxDepsLength)}
               , {60, L}
               ])
     || L <- Ls].

%% Given a Task, generate an arbitrary number of dependencies for that task
generate_deps(Task, [], _) ->
    Task;
generate_deps(Task, List, MaxDepsLength) ->
    Length = length(List),
    DepsLength = min(rand:uniform(Length), MaxDepsLength),
    %% Take arbitrary tasks from list
    Deps = [lists:nth(rand:uniform(Length), List)
            || _ <- lists:seq(1, DepsLength)],
    DepsNames = [N || #{<<"name">> := N} <- Deps],
    Task#{<<"requires">> => DepsNames}.
