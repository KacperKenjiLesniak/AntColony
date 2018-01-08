-module(simulation_pheromone_supervisor).
-behavior(supervisor).

-include("records.hrl").

-export([ start_link/1, init/1 ]).
-export([ kill_children/0, placePheromone/3 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(_State) ->
    simulation_event_stream:component_ready(?MODULE),
    {ok, {{one_for_one, 3, 60}, []}}.

placePheromone(WorldParameters, Position, FoodPosition) ->
    Pheromone = { {pheromone, Position},
                  {simulation_entity_pheromone, start_link, [{WorldParameters, Position, FoodPosition}]},
                  temporary, brutal_kill, worker,
                  [simulation_entity_pheromone]},
    supervisor:start_child(?MODULE, Pheromone).

kill_children() ->
    simulation_common:stop_children(?MODULE).
