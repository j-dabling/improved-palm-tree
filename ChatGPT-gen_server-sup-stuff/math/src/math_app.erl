%%%-------------------------------------------------------------------
%% @doc math public API
%% @end
%%%-------------------------------------------------------------------

-module(math_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    math_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
