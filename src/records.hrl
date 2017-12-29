-define(FOOD_QUANTITY, 5).

-record(target, {x, y}).
-record(position, {x, y}).

-record(food, {pid, world, position, quantity}).

-record(ant, {pid, position, colony_position, world, target = #target{},
                    food_around = false}).

-record(world_parameters, {food, ants, colonies, width, height}).
