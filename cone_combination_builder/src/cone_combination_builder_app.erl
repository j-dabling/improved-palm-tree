%%%-------------------------------------------------------------------
%% @doc cone_combination_builder public API
%% @end
%%%-------------------------------------------------------------------

-module(cone_combination_builder_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cone_combination_builder_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
