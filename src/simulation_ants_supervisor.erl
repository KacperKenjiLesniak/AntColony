-module(simulation_ants_supervisor).
-behavior(supervisor).

-include("records.hrl").

-export([ start_link/1, init/1 ]).
-export([ breed/1, kill_children/0 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(State) ->
    simulation_event_stream:component_ready(?MODULE),

    {ok, {{one_for_one, State#world_parameters.ants, 1}, []}}.

breed(Parameters) ->
    breedAnts(0, Parameters#world_parameters.ants, Parameters).

breedAnts(Created, Total, WorldParameters) when Created < Total ->
    Ant = { {ant, Created + 1},
               {simulation_entity_ant, start_link, [ {WorldParameters, random} ]},
               temporary, brutal_kill, worker,
               [ simulation_entity_ant ]},

    supervisor:start_child(?MODULE, Ant),
    breedAnts(Created + 1, Total, WorldParameters);

breedAnts(_Created, _Total, _WorldParameters) ->
    done.

kill_children() ->
    simulation_common:stop_children(?MODULE).
