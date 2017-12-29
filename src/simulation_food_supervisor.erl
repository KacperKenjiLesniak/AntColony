-module(simulation_food_supervisor).
-behavior(supervisor).

-include("records.hrl").

-export([ start_link/1, init/1 ]).
-export([ place/1, kill_children/0 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(State) ->
    simulation_event_stream:component_ready(?MODULE),
    {ok, {{one_for_one, State#world_parameters.food, 1}, []}}.

place(Parameters) ->
    placeFood(0, Parameters#world_parameters.food, Parameters).

placeFood(Created, Total, WorldParameters) when Created < Total ->
    Food = { {food, Created + 1},
               {simulation_entity_food, start_link, [ WorldParameters ]},
               temporary, brutal_kill, worker,
               [ simulation_entity_food ]},

    supervisor:start_child(?MODULE, Food),
    placeFood(Created + 1, Total, WorldParameters);

placeFood(_Created, _Total, _WorldParameters) ->
    done.

kill_children() ->
    simulation_common:stop_children(?MODULE).
