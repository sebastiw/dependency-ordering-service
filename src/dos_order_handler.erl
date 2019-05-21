-module(dos_order_handler).

-behaviour(cowboy_rest).

-export([ init/2
        , allowed_methods/2
        , content_types_accepted/2
        ]).

-export([ from_json/2
        ]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

from_json(Req0, State) ->
    {ok, JsonBinary, Req1} = cowboy_req:read_body(Req0),
    Req2 = cowboy_req:set_resp_body(JsonBinary, Req1),
    {true, Req2, State}.
