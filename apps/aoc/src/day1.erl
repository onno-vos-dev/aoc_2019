-module(day1).

-export([ part1/0
        , part2/0
        ]).

part1() ->
  calc(fun(I) -> fu(I) end).

part2() ->
  calc(fun(I) -> extra_fuel(fu(I), 0) end).

calc(F) ->
  lists:sum(lists:map(F, input())).

extra_fuel(Fuel, Acc) when Fuel =< 0 -> Acc;
extra_fuel(Fuel, Acc) -> extra_fuel(fu(Fuel), Acc + Fuel).

input() ->
  {ok, Bin} = util:read_file("day1.txt"),
  [ binary_to_integer(S) || S <- binary:split(Bin, <<"\n">>, [trim, global]) ].

fu(Fuel) ->
  {ok, I} = aoc:fuel(Fuel),
  I.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
