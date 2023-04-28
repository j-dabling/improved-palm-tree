-module(math).
-export([fac/1]).
-include_lib("eunit/include/eunit.hrl").

%%
%%The factorial function
%%
fac(0) -> 1;
fac(N) -> N * fac(N-1).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

fac_test_() ->
      [
       ?_assertEqual(1,fac(0)),
       ?_assertEqual(1,fac(1)),
       ?_assertEqual(2,fac(2)),
       ?_assertEqual(6,fac(3)),
       ?_assertEqual(24,fac(4)),
       ?_assertEqual(120,fac(5)),
       ?_assertEqual(93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000,
                        fac(100))
      ].
-endif.

