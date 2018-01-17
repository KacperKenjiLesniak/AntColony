-module(simulation_entity_ant).
-behavior(gen_statem).

-include("records.hrl").

-export([ start_link/1, init/1, terminate/3, callback_mode/0, handle_info/3, code_change/4 ]).
-export([ running/3]).

start_link(InitialState) ->
    gen_statem:start_link(?MODULE, InitialState, []).

init({WorldParameters, Position}) ->

    State = #ant{pid = self(), position = Position, colony_position = Position,
                    world = WorldParameters},

    simulation_event_stream:notify(ant, born, State),

    {ok, running, State, ?TIMEOUT}.

terminate(_, _StateName, State) ->
    simulation_event_stream:notify(ant, died, State),
    ok.

callback_mode() ->
    state_functions.

handle_info(stop_entity, _StateName, State) ->
    {stop, normal, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


running(timeout, _, State) ->

    NewPosition = simulation_common:next_position_to_target(State#ant.world, State#ant.position, State#ant.target),

    AskForTarget = case State#ant.target of
        {target, undefined, undefined} ->
            true;
        {target, X, Y} when X == NewPosition#position.x,
                            Y == NewPosition#position.y ->
            true;
        {target, _X, _Y} ->
            false
    end,

    NewState = case AskForTarget of
        true  ->

            EntityMet = what_is_at(NewPosition, State#ant.colony_position),

            PheromoneNear = pheromone_near(NewPosition),

            affect_target(EntityMet),

            {NewTarget, FoodPosition} = get_new_target(State, EntityMet, NewPosition, PheromoneNear),

            State#ant{position = NewPosition, target = NewTarget, food_position = FoodPosition};
        false ->

            State#ant{position = NewPosition}
    end,

    if
        State#ant.food_position /= undefined, NewState#ant.colony_position /= NewState#ant.position ->
            simulation_pheromone_supervisor:placePheromone(State#ant.world, State#ant.position, State#ant.food_position);
        true ->
            false
    end,

    simulation_event_stream:notify(ant, move, NewState),
    {next_state, running,NewState,?TIMEOUT}.

get_new_target(State, {nothing}, _NewPosition, {position, X, Y}) ->
    simulation_event_stream:notify(ant, pheromone_noticed, State),
    {#target{x = X, y = Y}, undefined};

get_new_target(_State, {nothing}, _NewPosition, _Pheromone) ->
    {#target{x = undefined, y = undefined}, undefined};

get_new_target(_State, {colony, _ColonyPosition}, _NewPosition, _Pheromone) ->
  {#target{x = undefined, y = undefined}, undefined};

get_new_target(State, {food, _Food}, NewPosition, _Pheromone) ->
    {#target{x = State#ant.colony_position#position.x, y = State#ant.colony_position#position.y}, NewPosition};

get_new_target(State, {pheromone, Pheromone}, _NewPosition, _Pheromone) ->
  FoodPosition = where_is_food(Pheromone),
  simulation_event_stream:notify(ant, following_pheromone, State),
  {#target{x = FoodPosition#position.x, y = FoodPosition#position.y}, undefined}.

affect_target({food, Food}) ->
  gen_server:call(Food, {eat});

affect_target(_) ->
  ok.

what_is_at(Position, ColonyPosition) when Position#position.x == ColonyPosition#position.x,
                                          Position#position.y == ColonyPosition#position.y ->
  {colony, ColonyPosition};

what_is_at(Position, _ColonyPosition) ->

  AllFood = supervisor:which_children(simulation_food_supervisor),
  AllPheromone = supervisor:which_children(simulation_pheromone_supervisor),
  what_is_at(Position, AllFood, AllPheromone).

what_is_at(Position, [{_Id, Food, _Type, _Modules} | Rest ], Pheromones) ->

  try gen_server:call(Food, {are_you_at, Position}) of
    true ->
        {food, Food};
    false ->
        what_is_at(Position, Rest, Pheromones)
  catch
    exit: _Reason -> what_is_at(Position, Rest)

end;

what_is_at(Position, [], [{_Id, Pheromone, _Type, _Modules} | Rest ]) ->

  try gen_server:call(Pheromone, {are_you_at, Position}) of
    true ->
        {pheromone, Pheromone};
    false ->
        what_is_at(Position, [], Rest)
  catch
    exit: _Reason -> what_is_at(Position, Rest)

end;

what_is_at(_Position, [], []) ->
    {nothing}.


pheromone_near(Position) ->
    AllPheromone = supervisor:which_children(simulation_pheromone_supervisor),
    pheromone_near(Position, AllPheromone).


pheromone_near(Position, [{_Id, Pheromone, _Type, _Modules} | Rest ]) ->
    try gen_server:call(Pheromone, {are_you_near, Position}) of
      {position, X, Y} ->
          #position{x = X, y = Y};
      false ->
          pheromone_near(Position, Rest)
    catch
      exit: _Reason -> pheromone_near(Position, Rest)
  end;

 pheromone_near(_Position, []) ->
      {nothing}.

 where_is_food(Pheromone) ->
   try gen_server:call(Pheromone, {where_is_food}) of
      FoodPosition -> FoodPosition
   catch
     exit: _Reason -> where_is_food(Pheromone)
   end.
