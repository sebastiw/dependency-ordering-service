-module(dos).

-export([ start/0
        , stop/0
        ]).

start() ->
    application:ensure_all_started(dos).

stop() ->
    application:stop(dos).
