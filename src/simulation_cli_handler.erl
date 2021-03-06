-module(simulation_cli_handler).
-behavior(gen_event).

-export([ init/1, handle_event/2,
          terminate/2, handle_call/2, handle_info/2, code_change/3 ]).

init(_Args) ->
    {ok, []}.

handle_event(Msg, State) ->
    Indicator = case Msg of
        {_, placed, _} -> "[++]";
        {_, born, _}    -> "[++]";

        {_, eaten, _}   -> "[--]";
        {_, died, _}    -> "[--]";
        {_, finished, _}-> "[--]";

        _               -> "[ii]"
    end,
    io:format("~s ~w ~n", [Indicator, Msg ]),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, empty, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
