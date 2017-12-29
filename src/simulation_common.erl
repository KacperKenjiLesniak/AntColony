-module(simulation_common).

-include("records.hrl").

-export([stop_children/1, next_position/2, valid_position/2]).

stop_children(SupervisorName) ->
    [ Pid ! stop_entity || {_, Pid, _, _} <- supervisor:which_children(SupervisorName) ].

next_position(World, Position) ->
    {X, Y} = {Position#position.x, Position#position.y},
    Direction = rand:uniform(8),
    case Direction of
        1  -> NewPosition = #position{x=X, y=Y+1};
        2  -> NewPosition = #position{x=X, y=Y-1};
        3  -> NewPosition = #position{x=X+1, y=Y};
        4  -> NewPosition = #position{x=X-1, y=Y};
        5 -> NewPosition = #position{x=X-1, y=Y+1};
        6 -> NewPosition = #position{x=X+1, y=Y+1};
        7 -> NewPosition = #position{x=X-1, y=Y-1};
        8 -> NewPosition = #position{x=X+1, y=Y-1}
    end,
    Valid = valid_position(World, NewPosition),
    if
        Valid == true ->
            NewPosition;
        true ->
            next_position(World, Position)
    end.

valid_position(World, Position) ->
    X = Position#position.x,
    Y = Position#position.y,
    if
        X > 0, Y >0, X < World#world_parameters.width, Y < World#world_parameters.height ->
            true;
        true ->
            false
    end.
