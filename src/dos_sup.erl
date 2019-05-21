-module(dos_sup).

-behaviour(supervisor).

%% Supervisor callback
-export([ init/1
        ]).

%% Management API
-export([ start_link/0
        ]).

%% Supervisor

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Childs = [],

    Strategy = #{ strategy => one_for_one
                , intensity => 10
                , period => 10
                },

    {ok, {Strategy, Childs}}.

