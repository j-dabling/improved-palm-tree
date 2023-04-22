%%%-------------------------------------------------------------------
%% @doc arithmatic public API
%% @end
%%%-------------------------------------------------------------------

-module(arithmatic_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    arithmatic_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
