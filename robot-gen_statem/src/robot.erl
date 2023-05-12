%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This state machine tracks the state of an industrial robot
%%% that is programmed to pick up computers from an invariat
%%% source location and place the computer in a box that is 
%%% also in an invariant location. The robot knows nothing
%%% about where the computers come from nor where they go.
%%% @end

%%% Created : 24 June 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(robot).
-behaviour(gen_statem).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/2,start_link/2,stop/1,hand_open_to_down/1,hand_down_to_close/1,hand_close_to_up/1]).

%% Supervisor Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
%% State Callbacks
-export([handle_event/4]).


%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Starts a robot process. Since it doesn't link to the process 
%% that started it, it can not be used by a supervisor to start a
%% robot.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),term()) -> {ok, atom()}.
start(Statem_name,Initial_state) ->
    gen_statem:start({local,Statem_name}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Starts a robot process and links it to its calling process. This is
%% the API function a supervisor would use.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(),term()) -> {ok, atom()}.
start_link(Statem_name,Initial_state) ->
    gen_statem:start_link({local,Statem_name},?MODULE,Initial_state,[]).


%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the robot.
%%
%% The parameter of stop is an atom that
%% is a registered name of a robot.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
    gen_statem:stop(Statem_name).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
    void.
%% @private
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
%% @private
init(Worker_ids) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    {ok,up,Worker_ids}.
%%
%% This robot will use the handle_event_function pattern.
%% @private
callback_mode() -> handle_event_function.

%%% state changing API Functions

hand_open_to_down(Boxer)->  % Boxer is Statem_name.
    gen_statem:call(Boxer,to_down).

hand_down_to_close(Boxer) ->
    gen_statem:call(Boxer,to_close).

hand_close_to_up(Boxer) ->
    gen_statem:call(Boxer,to_up).

%%
%% Used to put the robot state machine in its next state.
%% @private

handle_event({call,From}, to_down, up,{Statem_name,State_data}) ->
    %tell robot to move down
    io:format("open hand to down "),
    {next_state, down,{down,State_data},[{reply,From,is_down}]};

handle_event({call,From}, to_close, down,{Statem_name,State_data}) ->
    %tell robot to move down
    io:format("down to close hand "),
    {next_state, closed,{closed,State_data},[{reply,From,is_closed}]};

handle_event({call,From}, to_up, closed,{Statem_name,State_data}) ->
    %tell robot to move down
    io:format(" closed to up "),
    {next_state, up,{up,State_data},[{reply,From,is_up}]};


%
% a bunch of other state-to-state changes go here.
%
handle_event({call,From},Attemped_state,Current_state,{Statem_name,State_data}) ->
    io:format("Current state, ~p, does not allow a change to ~p state~n",[Current_state,Attemped_state]),
    {next_state,Current_state,{Current_state,State_data},[{reply,From,fail}]}.


%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
-endif.
