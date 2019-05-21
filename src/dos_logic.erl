-module(dos_logic).

-export([ order/1
        , check_no_duplicates/1
        , check_dependencies_exist/1
        , check_no_self_references/1
        , get_ordered_tasks/1
        ]).

order(Tasks) ->
    Checks = [ {fun check_no_duplicates/1, duplicates}
             , {fun check_dependencies_exist/1, dependency_does_not_exist}
             , {fun check_no_self_references/1, self_reference}
             ],
    case do_checks(Checks, Tasks) of
        ok ->
            get_ordered_tasks(Tasks);
        Err ->
            Err
    end.

do_checks([], _) ->
    ok;
do_checks([{CheckFun, Error}|Funs], Tasks) ->
    case CheckFun(Tasks) of
        false ->
            {error, Error};
        true ->
            do_checks(Funs, Tasks)
    end.

check_no_duplicates(Tasks) ->
    Names = get_all_names(Tasks),
    length(Names) == length(lists:usort(Names)).

check_dependencies_exist(Tasks) ->
    Names = get_all_names(Tasks),
    Deps = get_all_requires(Tasks),
    lists:all(fun (B) -> B end, [lists:member(D, Names) || D <- Deps]).

check_no_self_references(Tasks) ->
    lists:all(fun (B) -> B end,
              [not lists:member(Name, Deps)
               || #{<<"name">> := Name, <<"requires">> := Deps} <- Tasks]).

get_all_names(Tasks) ->
    [N || #{<<"name">> := N} <- Tasks].

get_all_requires(Tasks) ->
    lists:append([Rs || #{<<"requires">> := Rs} <- Tasks]).

get_ordered_tasks(Tasks) ->
    G = build_dep_tree(Tasks),
    case digraph_utils:topsort(G) of
        false ->
            {error, no_topological_order_exist};
        Sorted ->
            Reversed = lists:reverse(Sorted),
            Ordered = lists:map(fun (V) ->
                                        {_, L} = digraph:vertex(G, V),
                                        L
                                end,
                                Reversed),
            {ok, Ordered}
    end.

build_dep_tree(Tasks) ->
    G = digraph:new(),
    add_vertices(G, Tasks),
    add_edges(G, Tasks),
    G.

add_vertices(_G, []) ->
    ok;
add_vertices(G, [T|Tasks]) ->
    #{<<"name">> := Name} = T,
    digraph:add_vertex(G, Name, T),
    add_vertices(G, Tasks).

add_edges(_G, []) ->
    ok;
add_edges(G, [T|Tasks]) ->
    #{<<"name">> := Name} = T,
    Deps = maps:get(<<"requires">>, T, []),
    [digraph:add_edge(G, Name, D) || D <- Deps],
    add_edges(G, Tasks).
