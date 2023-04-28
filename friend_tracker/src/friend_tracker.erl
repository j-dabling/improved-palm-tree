-module(friend_tracker).
-export([start/1, rpc/2, run/1]).

%%
%% Spawn a process for adding, removing, and finding friends.
%% The parameter is an initial list of friends. It may be an
%% empty list.
%%

start(Initial_friends_list) ->
    %The MODULE macro is used instead of hard coding the module name.
    spawn(?MODULE, run, [Initial_friends_list]).

%%--------------------------
%% Client functions
%%--------------------------

%%
%% Add, remove, or find a friend.
%%
rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        Response ->
            Response
    end.

add(Friend, Friend_list) -> [Friend] ++ Friend_list;
add(List, Friend_list) -> List ++ Friend_list.

remove(Friend, Friend_list) -> Friend_list -- [Friend];
remove(List, Friend_list) -> Friend_list -- List.

has_friend(Friend, Friend_list) ->
    lists:member(Friend, Friend_list).

has_friends([], Friend_list)-> true;
has_friends([First|Rest], Friend_list)->
	case lists:member(First, Friend_list) of
		true -> true andalso has_friends(Rest, Friend_list);
		false -> false
	end.

%%---------------------------
%% Server function
%%---------------------------

run(Friend_list)->
	receive
		% {Pid, {command, data}}-> do_stuff
		{Pid, {add, Data}}-> add(Data, Friend_list), Pid ! received;
		{Pid, {remove, Data}}-> remove(Data, Friend_list), Pid ! received;
		{Pid, {has_friend, Friend}}-> Pid ! has_friend(Friend, Friend_list);
        % {Pid, {has_friend, Friend}}-> Pid ! true;
		{Pid, {has_friends, List}}-> Pid ! has_friends(List, Friend_list);
        {Pid, get}-> Pid ! Friend_list;
        {Pid,_ } -> Pid ! {fail, unrecognized_message}
		% {Pid, {command, data}}-> do_stuff
	end,
	run(Friend_list).

-ifdef(EUNIT).
%%
%% Unit tests go here.
%%

-include_lib("eunit/include/eunit.hrl").

add_friend_test_() ->
    {setup,
        %runs before any of the tests
        fun() ->
            Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
            register(test_adder, Pid)
        end,
        %runs after all of the tests
        fun(_) ->
            unregister(test_adder)
        end,
        %factorializer tests start here

        %happy path
        [
            ?_assertEqual(received, rpc(test_adder, {add, bob})),
            %nasty thoughts start here
            ?_assertEqual(received, rpc(test_adder, {add, 1})),
            ?_assertEqual(received, rpc(test_adder, {add, #{name => suzannah, age => 23}}))
        ]}.

add_friends_test_() ->
    {setup,
        %runs before any of the tests
        fun() ->
            Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
            register(test_adder, Pid)
        end,
        %runs after all of the tests
        fun(_) ->
            unregister(test_adder)
        end,
        %factorializer tests start here

        %happy path
        [
            ?_assertEqual(received, rpc(test_adder, {add, [bob, alice, joe]})),
            %nasty thoughts start here
            ?_assertEqual(received, rpc(test_adder, {add, []})),
            ?_assertEqual(
                received,
                rpc(
                    test_adder,
                    {add, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]}
                )
            )
        ]}.

has_friend_test_() ->
    {setup,
        %runs before any of the tests
        fun() ->
            Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
            register(test_finder, Pid)
        end,
        %runs after all of the tests
        fun(_) ->
            unregister(test_finder)
        end,
        %factorializer tests start here

        %happy path
        [
            ?_assert(rpc(test_finder, {has_friend, sue})),
            %nasty thoughts start here
            ?_assertNot(rpc(test_finder, {has_friend, bob})),
            ?_assertNot(
                rpc(
                    test_finder,
                    {has_friend, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]}
                )
            )
        ]}.

has_friends_test_() ->
    {setup,
        %runs before any of the tests
        fun() ->
            Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
            register(test_finder, Pid)
        end,
        %runs after all of the tests
        fun(_) ->
            unregister(test_finder)
        end,
        %factorializer tests start here

        %happy path
        [
            ?_assert(rpc(test_finder, {has_friends, [sue, fred]})),
            %nasty thoughts start here
            ?_assert(rpc(test_finder, {has_friends, []})),
            ?_assertNot(rpc(test_finder, {has_friends, [bob]})),
            ?_assertNot(
                rpc(
                    test_finder,
                    {has_friends, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]}
                )
            )
        ]}.

remove_friend_test_() ->
    {setup,
        %runs before any of the tests
        fun() ->
            Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
            register(test_remover, Pid)
        end,
        %runs after all of the tests
        fun(_) ->
            unregister(test_remover)
        end,
        %factorializer tests start here

        %happy path
        [
            ?_assertEqual(received, rpc(test_remover, {remove, fred})),
            %nasty thoughts start here
            ?_assertEqual(received, rpc(test_remover, {remove, bob})),
            ?_assertEqual(
                received,
                rpc(
                    test_remover,
                    {remove, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]}
                )
            )
        ]}.

remove_friends_test_() ->
    {setup,
        %runs before any of the tests
        fun() ->
            Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
            register(test_remover, Pid)
        end,
        %runs after all of the tests
        fun(_) ->
            unregister(test_remover)
        end,
        %factorializer tests start here

        %happy path
        [
            ?_assertEqual(received, rpc(test_remover, {remove, [sue, fred]})),
            %nasty thoughts start here
            ?_assertEqual(received, rpc(test_remover, {remove, [bob]})),
            ?_assertEqual(
                received,
                rpc(
                    test_remover,
                    {remove, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]}
                )
            )
        ]}.

get_friends_test_() ->
    {setup,
        %runs before any of the tests
        fun() ->
            Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
            register(test_remover, Pid)
        end,
        %runs after all of the tests
        fun(_) ->
            unregister(test_remover)
        end,
        %factorializer tests start here

        %happy path
        [?_assertEqual([sue, grace, fred], rpc(test_remover, get))]}.

bad_message_test_() ->
    {setup,
        %runs before any of the tests
        fun() ->
            Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
            register(test_bad_message, Pid)
        end,
        %runs after all of the tests
        fun(_) ->
            unregister(test_bad_message)
        end,
        %factorializer tests start here

        %happy path
        [
            ?_assertMatch({fail, unrecognized_message}, rpc(test_bad_message, what)),
            ?_assertMatch({fail, unrecognized_message}, rpc(test_bad_message, nil)),
            ?_assertMatch({fail, unrecognized_message}, rpc(test_bad_message, [])),
            ?_assertMatch({fail, unrecognized_message}, rpc(test_bad_message, {}))
        ]}.

-endif.
