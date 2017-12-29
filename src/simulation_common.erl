-module(simulation_common).

-include("records.hrl").

-export([stop_children/1 ]).

stop_children(SupervisorName) ->
    [ Pid ! stop_entity || {_, Pid, _, _} <- supervisor:which_children(SupervisorName) ].
