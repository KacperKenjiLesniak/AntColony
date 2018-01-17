{ application, ant_colony_simulation,
 [ { description, "World simulation in Erlang with Ants and Food" },
   { vsn, "1.0" },
   { modules, [ simulation, simulation_main_supervisor, simulation_simulations_supervisor, simulation_cli_handler,
   simulation_controller, simulation_event_stream, simulation_food_supervisor,
   simulation_ants_supervisor, simulation_common, simulation_entity_food, simulation_entity_ant, simulation_pheromone_supervisor, simulation_entity_pheromone] },
   { registered, [] },
   { applications, [ kernel, stdlib] },
   { env, [] },
   { mod, { simulation, [] } }
 ]
}.
