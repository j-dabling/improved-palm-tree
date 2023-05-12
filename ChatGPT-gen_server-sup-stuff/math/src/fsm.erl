-module(fsm).
-behavior(gen_statem).
-export([start_link/0, init/1, fac/1, add_state/0, add_them/2, handle_sync_event/4, terminate/3,selection/3,addition/3, display/2, callback_mode/0]).

start_link() ->
    gen_statem:start_link({local,fsm}, ?MODULE, [], []).

init([]) ->
    {ok,selection,[]}.

callback_mode() ->
    state_functions.

% events
fac(Number) -> gen_statem:cast(?MODULE,{selection, factorial, Number}).
add_state() ->gen_statem:cast(?MODULE,add).

add_them(Num1,Num2) -> ?MODULE ! {add_them, Num1, Num2}.

% 
selection({cast,fsm},{selection, factorial, Number},_Loopdata) ->
    io:format("In the Selection State ~n"),
    Total = gen_server:call(math_1,{fac,Number}),
    {next_state, display, Total};

selection({cast,_From},add,_Loopdata) ->
    {next_state, addition,_Loopdata};

selection(_,_Other,_Loopdata) ->   % cancel
	{next_state, selection,_Loopdata}.

addition(_,{add_them, Num1, Num2}, _Loopdata) ->
    io:fwrite("Add State ~n"),
            Total = Num1 + Num2,
            {next_state, display, Total};
addition(_, _Other, _Loopdata) ->  % cancel
	        {next_state, selection,_Loopdata}.

display(Total,_Loopdata) ->
    io:fwrite("Number: ~p~n", [Total]),
    {next_state, selection,_Loopdata,[{reply,fsm,Total}]}.

handle_sync_event(stop, _From, _StateName, Data) ->
    {stop, normal, ok, Data}.

terminate(_Reason, _State, _Data) ->
    ok.


