-define(FOOD_QUANTITY, 5).
-define(PHEROMONE_TIME, 10 000).
-define(TIMEOUT, 3 000).

-record(target, {x, y}).
-record(position, {x, y}).

-record(food, {pid, world, position, quantity}).

-record(pheromone, {pid, position, food_position, world}).

-record(ant, {pid, position, colony_position, world, target = #target{},
                    food_around = false}).

-record(world_parameters, {food, ants, colonies, width, height}).

-record(colony, {position, ants_number}).
