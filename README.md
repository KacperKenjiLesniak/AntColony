# AntColony
Erlang simulation of ant colony.

To start simulation:
rebar3 compile
rebar3 shell
observer:start(),application:start(ant_colony_simulation),simulation_controller:start_simulation().
