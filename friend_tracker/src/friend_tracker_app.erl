%%%-------------------------------------------------------------------
%% @doc friend_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(friend_tracker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    friend_tracker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
