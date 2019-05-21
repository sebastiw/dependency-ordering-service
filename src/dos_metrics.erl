-module(dos_metrics).

-export([ setup/0
        , inc/2
        ]).

setup() ->
    Counters = [ { successful_requests
                 , "Number of successful requests and the length of "
                   "the tasks within that request."
                 , [task_length]
                 }
               , { failed_requests
                 , "Number of error responses and the type of failure."
                 , [type]
                 }
               ],
    [prometheus_counter:declare([{name, N}, {help, H}, {labels, L}])
     || {N, H, L} <- Counters].

inc(Name, Labels) ->
    prometheus_counter:inc(Name, Labels).

