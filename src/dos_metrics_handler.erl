-module(dos_metrics_handler).

-export([ init/2
        ]).

init(Req0, State) ->
    Body = prometheus_text_format:format(),
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, State}.
