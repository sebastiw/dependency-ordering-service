-module(dos_app).

-behaviour(application).

-export([ start/2
        , stop/1
        ]).

start(normal, []) ->
    {ok, _} = start_cowboy(),
    dos_metrics:setup(),
    {ok, _} = dos_sup:start_link().

stop(_) ->
    cowboy:stop_listener(dos_http_listener).


start_cowboy() ->
    Routes = [ {'_', [ {"/order", dos_order_handler, #{}}
                     , {"/metrics", dos_metrics_handler, #{}}
                     ]
               }
             ],
    Dispatch = cowboy_router:compile(Routes),
    Port = application:get_env(dos, http_port, 8000),
    cowboy:start_clear( dos_http_listener
                      , [{port, Port}]
                      , #{env => #{dispatch => Dispatch}}
                      ).
