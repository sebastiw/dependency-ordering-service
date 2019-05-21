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
    case try_order_tasks(JsonBinary) of
        {ok, Ordered} ->
            {ok, Response} = format_output(Ordered, <<"json">>),
            Req2 = cowboy_req:set_resp_body(Response, Req1),
            {true, Req2, State};
        {error, _} = Err ->
            Req2 = fail_request(Err, Req1),
            {false, Req2, State}
    end.

try_order_tasks(JsonBinary) ->
    case jsx:is_json(JsonBinary) of
        true ->
            case jsx:decode(JsonBinary, [return_maps]) of
                #{<<"tasks">> := Tasks} ->
                    dos_logic:get_ordered_tasks(Tasks);
                _ ->
                    {error, no_tasks_found}
            end;
        false ->
            {error, not_json_data}
    end.

format_output(Tasks, <<"json">>) ->
    ValidKeys = [<<"name">>, <<"command">>],
    Stripped = [maps:with(ValidKeys, T) || T <- Tasks],
    {ok, jsx:encode(Stripped)}.
fail_request({error, E} = Err, Req1) ->
    Response = format_error(Err),
    cowboy_req:set_resp_body(Response, Req1).

format_error({error, not_json_data}) ->
    <<"Error: Data provided is not correctly formatted as JSON.">>;
format_error({error, no_tasks_found}) ->
    <<"Error: The JSON provided does not include any tasks.">>;
format_error({error, no_topological_order_exist}) ->
    <<"Error: Impossible to prepare an order of the tasks.">>.
