-module(dos_logic).

-export([ get_ordered_tasks/1
        ]).

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
