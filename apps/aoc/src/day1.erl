-module(day1).

-export([part1/0]).
-export([part2/0]).

-define(fuel(I), (I div 3) - 2).

part1() ->
  calc(fun(I) -> ?fuel(I) end).

part2() ->
  calc(fun(I) -> extra_fuel(?fuel(I), 0) end).

calc(F) ->
  lists:sum(lists:map(F, input())).

extra_fuel(Fuel, Acc) when Fuel =< 0 -> Acc;
extra_fuel(Fuel, Acc) -> extra_fuel(?fuel(Fuel), Acc + Fuel).

input() ->
  {ok, Bin} = util:read_file("day1.txt"),
  [ list_to_integer(S) || S <- string:tokens(binary_to_list(Bin), "\n") ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
