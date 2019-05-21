-module(dos_app).

-behaviour(application).

-export([ start/2
        , stop/1
        ]).

start(normal, []) ->
    {ok, nothing}.

stop(_) ->
    ok.
