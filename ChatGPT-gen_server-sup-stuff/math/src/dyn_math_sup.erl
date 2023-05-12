-module(dyn_math_sup).
-behavior(supervisor).

-export([start_link/0, init/1,generate/1,ss/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ss().


init([]) ->
    {ok, {{one_for_one, 10, 3600}, []}}.

generate(Ms) ->

            ChildSpec = {Ms, {math, start_link, [Ms]},
                         temporary, 2000, worker, [math]},
            supervisor:start_child(?MODULE, ChildSpec).
ss() ->
    generate(math_1),
    generate(math_2),
    generate(math_3),
    generate(math_4),
    generate(math_5),
    generate(math_6),
    generate(math_7),
    generate(math_8),
    generate(math_9),
    generate(math_10).
