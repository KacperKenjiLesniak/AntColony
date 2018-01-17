-define(FOOD_QUANTITY, 5).
-define(PHEROMONE_TIME, 24000).
-define(TIMEOUT, 3000).

-record(target, {x, y}).
-record(position, {x, y}).

-record(food, {pid, world, position, quantity}).

-record(pheromone, {pid, position, food_position, world}).

-record(ant, {pid, position, colony_position, world, target = #target{},
                    food_around = false, food_position = undefined}).

-record(world_parameters, {food, ants, colonies, width, height}).

-record(colony, {position, ants_number}).
