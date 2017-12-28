{ application, ant_colony_simulation,
 [ { description, "World simulation in Erlang with Ants and Food" },
   { vsn, "1.0" },
   { modules, [ simulation, simulation_main_supervisor, simulation_simulations_supervisor, simulation_controller] },
   { registered, [] },
   { applications, [ kernel, stdlib, sasl ] },
   { env, [] },
   { mod, { simulation, [] } }
 ]
}.
