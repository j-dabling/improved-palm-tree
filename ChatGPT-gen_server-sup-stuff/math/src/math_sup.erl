%%%-------------------------------------------------------------------
%% @doc math top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(math_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1,stop/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE},?MODULE, []).

stop() -> 
    exit(whereis(math_sup), shutdown).

init(_) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 2,
                 period => 1000},
    ChildSpecs = #{id => dyn_math_sup,       
                 start => {dyn_math_sup,start_link,[]},      
                 restart => permanent,   
                 shutdown => 2000, 
                 type => supervisor,       
                 modules => [dyn_math_sup]},

    Rr_Spec = #{id => rr_selector,       
                 start => {rr_selector,start_link,[[math_1,math_2,math_3,math_4,math_5,math_6,math_7,math_8,math_9,math_10]]},      
                 restart => permanent,   
                 shutdown => 2000, 
                 type => worker,       
                 modules => [rr_selector]},

    {ok, {SupFlags, [ChildSpecs,Rr_Spec]}}.

%% internal functions
