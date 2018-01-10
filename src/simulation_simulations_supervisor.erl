-module(simulation_simulations_supervisor).
-behavior(supervisor).

-include("records.hrl").

-export([ start_link/1, init/1 ]).
-export([ populate/1, restart/0 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(WorldParameters) ->
    Args = [ WorldParameters ],

    PheromoneSupervisor = {pheromone_supervisor,
                         {simulation_pheromone_supervisor, start_link, Args},
                         permanent, brutal_kill, supervisor,
                         [ simulation_pheromone_supervisor ]},

    FoodSupervisor = {food_supervisor,
                         {simulation_food_supervisor, start_link, Args},
                         permanent, brutal_kill, supervisor,
                         [ simulation_food_supervisor ]},

    AntsSupervisor = {ants_supervisor,
                         {simulation_ants_supervisor, start_link, Args},
                         permanent, brutal_kill, supervisor,
                         [ simulation_ants_supervisor ]},


    {ok, {{one_for_all, 1, 60},
          [PheromoneSupervisor, FoodSupervisor, AntsSupervisor]}}.

populate(Parameters) ->
    simulation_food_supervisor:place(Parameters),
    simulation_ants_supervisor:breed(Parameters),
    done.

restart() ->
    simulation_food_supervisor:kill_children(),

    simulation_ants_supervisor:kill_children(),
    done.
