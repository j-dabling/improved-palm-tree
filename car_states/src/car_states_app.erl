%%%-------------------------------------------------------------------
%% @doc car_states public API
%% @end
%%%-------------------------------------------------------------------

-module(car_states_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    car_states_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
