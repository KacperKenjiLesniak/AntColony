-module(simulation_ants_supervisor).
-behavior(supervisor).

-include("records.hrl").

-export([ start_link/1, init/1 ]).
-export([ breed/1, kill_children/0 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(_State) ->
    simulation_event_stream:component_ready(?MODULE),

    {ok, {{one_for_one, 5, 1}, []}}.

breed(WorldParameters) ->
    CurriedBreed = fun(Colony) -> breedAnts(WorldParameters, Colony) end,
    lists:foreach(CurriedBreed, WorldParameters#world_parameters.colonies).

breedAnts(WorldParameters, Colony) ->
    breedAnts(Colony#colony.ants_number, Colony#colony.position, WorldParameters).

breedAnts(AntsLeft, ColonyPosition, WorldParameters) when AntsLeft > 0 ->
    Ant = { {ant, ColonyPosition, AntsLeft},
               {simulation_entity_ant, start_link, [ {WorldParameters, ColonyPosition} ]},
               temporary, brutal_kill, worker,
               [ simulation_entity_ant ]},

    supervisor:start_child(?MODULE, Ant),
    breedAnts(AntsLeft - 1, ColonyPosition, WorldParameters);

breedAnts(_AntsLeft, _ColonyPosition, _WorldParameters) ->
    done.

kill_children() ->
    simulation_common:stop_children(?MODULE).
