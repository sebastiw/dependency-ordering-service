-module(logic_SUITE).

-include_lib("eunit/include/eunit.hrl").

-define(TASK1, #{ <<"name">> => <<"task-1">>
                , <<"command">> => <<"touch /tmp/file1">>
                }).
-define(TASK2, #{ <<"name">> => <<"task-2">>
                , <<"command">> => <<"cat /tmp/file1">>
                , <<"requires">> => [<<"task-3">>]
                }).
-define(TASK3, #{ <<"name">> => <<"task-3">>
                , <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>
                , <<"requires">> => [<<"task-1">>]
                }).
-define(TASK4, #{ <<"name">> => <<"task-4">>
                , <<"command">> => <<"rm /tmp/file1">>
                , <<"requires">> => [<<"task-2">>, <<"task-3">>]
                }).

example_test() ->
    In = [ ?TASK1, ?TASK2, ?TASK3, ?TASK4 ],
    Out = [ ?TASK1, ?TASK3, ?TASK2, ?TASK4 ],
    ?assertEqual({ok, Out}, dos_logic:get_ordered_tasks(In)).

duplicate_test() ->
    In = [ ?TASK1, ?TASK1 ],
    Out = {error, duplicates},
    ?assertEqual(Out, dos_logic:get_ordered_tasks(In)).

dependencies_exist_test() ->
    In = [ ?TASK2 ],
    Out = {error, dependency_does_not_exist},
    ?assertEqual(Out, dos_logic:get_ordered_tasks(In)).

self_reference_test() ->
    Task1Name = maps:get(<<"name">>, ?TASK1),
    In = [ ?TASK1#{<<"requires">> => [Task1Name]} ],
    Out = {error, self_reference},
    ?assertEqual(Out, dos_logic:get_ordered_tasks(In)).

topological_order_test() ->
    Task3Name = maps:get(<<"name">>, ?TASK3),
    In = [ ?TASK1#{<<"requires">> => [Task3Name]}, ?TASK3 ],
    Out = {error, no_topological_order_exist},
    ?assertEqual(Out, dos_logic:get_ordered_tasks(In)).
