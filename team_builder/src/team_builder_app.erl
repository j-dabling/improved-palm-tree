%%%-------------------------------------------------------------------
%% @doc team_builder public API
%% @end
%%%-------------------------------------------------------------------

-module(team_builder_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    team_builder_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
