-module(math).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([fac/1]).

start_link(Ms) -> gen_server:start_link({local,Ms}, math,[],[]).

init(_) ->
    Fac = 0,
    {ok, Fac}.

fac(0) -> 1;
fac(N) -> N * fac(N-1).

handle_call({fac, Number}, _From, _Fac) ->
    % gen_server:reply(From, ok),
    NewFac = fac(Number),
    {reply, NewFac, NewFac}.

handle_cast(stop, Fac) ->
    {stop, normal, Fac}.

handle_info(_Info, Fac) ->
    {noreply, Fac}.

terminate(_Reason, _Fac) ->
    ok.