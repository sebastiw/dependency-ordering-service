-module(endpoint_SUITE).

-include_lib("eunit/include/eunit.hrl").

-define(TASK1, #{ <<"name">> => <<"task-1">>
                , <<"command">> => <<"touch /tmp/file1">>
                }).
-define(TASK2, #{ <<"name">> => <<"task-2">>
                , <<"command">> => <<"cat /tmp/file1">>
                , <<"requires">> => [<<"task-3">>]
                }).
-define(TASK3, #{ <<"name">> => <<"task-3">>
                , <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>
                , <<"requires">> => [<<"task-1">>]
                }).
-define(TASK4, #{ <<"name">> => <<"task-4">>
                , <<"command">> => <<"rm /tmp/file1">>
                , <<"requires">> => [<<"task-2">>, <<"task-3">>]
                }).


-define(INPUT, jsx:encode(#{tasks => [ ?TASK1, ?TASK2, ?TASK3, ?TASK4 ]})).

-define(TASK_ORDER, [?TASK1, ?TASK3, ?TASK2, ?TASK4]).

-define(OUTPUT,
        fun ("json") ->
                jsx:encode([maps:without([<<"requires">>], T)
                            || T <- ?TASK_ORDER ]);
            ("bash") ->
                iolist_to_binary(["#!/usr/bin/env bash\n\n",
                                  [[C, $\n] || #{<<"command">> := C} <- ?TASK_ORDER ]]
                                )
        end).


endpoints_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun (_S) ->
             [ order_endpoint("json")
             , order_endpoint("bash")
             ]
     end}.

setup() ->
    dos:start().

teardown(_) ->
    dos:stop().

order_endpoint(Format) ->
    Request = {order_url(Format), [], "Application/json", ?INPUT},
    {ok, {{_, StatusCode, _}, _H, Body}} = httpc:request(post, Request, [], []),
    [ { "Testing " ++ Format ++ " status code"
      , ?_assertEqual(200, StatusCode)}
    , { "Testing " ++ Format ++ " output"
      , ?_assertEqual(?OUTPUT(Format), list_to_binary(Body))}
    ].

base_url() ->
    Port = application:get_env(dos, http_port, 8000),
    "http://localhost:" ++ integer_to_list(Port).

order_url("json") ->
    base_url() ++ "/order";
order_url(Format) ->
    base_url() ++ "/order?format=" ++ Format.
