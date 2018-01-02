-module(simulation_entity_food).
-behavior(gen_server).

-include("records.hrl").

-export([ init/1, handle_call/3, terminate/2, code_change/3, handle_cast/2, handle_info/2 ]).
-export([ start_link/1 ]).

start_link(WorldParameters) ->
    gen_server:start_link(?MODULE, WorldParameters, []).

init(WorldParameters) ->

    Width = WorldParameters#world_parameters.width,
    Height = WorldParameters#world_parameters.height,

    {X, Y} =  {rand:uniform(Width - 1), rand:uniform(Height - 1)},
    State = #food{pid = self(),
                    world = WorldParameters,
                    position = #position{x = X, y = Y},
                    quantity = ?FOOD_QUANTITY},

    simulation_event_stream:notify(food, placed, State),
    {ok, State}.

handle_call({are_you_at, Position}, _From, State) ->
    {X, Y} = {(State#food.position)#position.x, (State#food.position)#position.y},
    Result = case {Position#position.x, Position#position.y} of
                 {X, Y} -> true;
                 _      -> false
             end,
    {reply, Result, State};

handle_call(eat, _From, State) ->
    case State#food.quantity of
        0 ->
            {stop, normal, {error, food_patch_eaten}, State};
        _ ->
            NewQuantity = State#food.quantity - 1,
            NewState = State#food{quantity = NewQuantity},
            simulation_event_stream:notify(food, bite, NewState),
            {reply, {ok, food_patch_partially_eaten}, NewState}
    end.

terminate(_, State) ->
    simulation_event_stream:notify(food, eaten, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(stop_entity, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.
