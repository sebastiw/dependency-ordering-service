-module(dos_app).

-behaviour(application).

-export([ start/2
        , stop/1
        ]).

start(normal, []) ->
    {ok, _} = dos_sup:start_link().

stop(_) ->
    ok.
