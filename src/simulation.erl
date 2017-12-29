-module(simulation).
-behavior(application).

-include("records.hrl").

-export([ start/2, stop/1 ]).

read_world_parameters_from_settings() ->
    Food = application:get_env(ant_colony_simulation, food, 3),

    Colonies = application:get_env(ant_colony_simulation, colonies, [#colony{position = #position{x = 5, y = 5}, ants_number = 4}]),
    Width = application:get_env(ant_colony_simulation, width, 70),
    Height = application:get_env(ant_colony_simulation, height, 70),

    #world_parameters{food = Food,
                      colonies = Colonies,
                      width = Width,
                      height = Height}.

start(_Type, _Args) ->
    Parameters = read_world_parameters_from_settings(),
    simulation_main_supervisor:start_link(Parameters).

stop(_State) ->
    ok.
