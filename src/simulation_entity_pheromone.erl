-module(simulation_entity_pheromone).
-behavior(gen_server).

-include("records.hrl").

-export([ init/1, handle_call/3, terminate/2, code_change/3, handle_cast/2, handle_info/2 ]).
-export([ start_link/1 ]).

start_link(InitialState) ->
    gen_fsm:start_link(?MODULE, InitialState, []).

init({WorldParameters, Position, FoodPosition}) ->

    State = #pheromone{pid = self(), position = Position, food_position = FoodPosition,
                    world = WorldParameters},

    simulation_event_stream:notify(pheromone, placed, State),
    erlang:send_after(PHEROMONE_TIME, self(), timeout_shutdown),
    {ok, State}.

handle_call({are_you_at, Position}, _From, State) ->
    {X, Y} = {(State#pheromone.position)#position.x, (State#pheromone.position)#position.y},
    Result = case {Position#position.x, Position#position.y} of
                 {X, Y} -> true;
                 _      -> false
             end,
    {reply, Result, State};

terminate(_, State) ->
    % simulation_event_stream:notify(pheromone, finished, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout_shutdown, State) ->
  simulation_event_stream:notify(pheromone, finished, State),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.
