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
    logger:info("Starting dos at port ~B", [Port]),
    cowboy:start_clear( dos_http_listener
                      , [{port, Port}]
                      , #{ env => #{dispatch => Dispatch}
                         , stream_handlers => [ cowboy_metrics_h
                                              , cowboy_stream_h
                                              ]
                         , metrics_callback => fun log_request/1
                         }
                      ).

log_request(Metrics) ->
    #{ req := Req
     , resp_status := RespStatus
     , req_start := ReqStart
     , resp_end := RespEnd
     } = Metrics,
    Uri = cowboy_req:uri(Req),
    Method = cowboy_req:method(Req),
    {Ip, _Port} = cowboy_req:peer(Req),
    IpAddr = inet:ntoa(Ip),
    UserAgent = cowboy_req:header( <<"user-agent">>, Req
                                 , <<"User-Agent not configured">>),
    Time = erlang:convert_time_unit(RespEnd - ReqStart, native, microsecond),
    logger:info( "~s -> ~s ~s ~B in ~Bu [~s]"
               , [IpAddr, Method, Uri, RespStatus, Time, UserAgent]).
