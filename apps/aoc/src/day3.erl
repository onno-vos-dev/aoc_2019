-module(day3).

-export([run/0]).

run() ->
  run(input()).

run([Wire1, Wire2]) ->
  Acc = maps:new(),
  Steps = 1,
  X = 0,
  Y = 0,
  NewAcc = trace_wire(wire1, Wire1, {{Steps, X, Y}, Acc}),
  Out = trace_wire(wire2, Wire2, {{Steps, X, Y}, NewAcc}),
  Crossed = find_crossed(Out),
  Part1 = part1(Crossed),
  Part2 = part2(Crossed),
  [{part1, Part1}, {part2, Part2}].

part1(Crossed) ->
  element(1, hd(Crossed)).

part2(Crossed) ->
  hd(lists:sort([ X + Y || {_, [{X,_},{Y,_}]} <- Crossed])).

%%%_* Internal =================================================================
trace_wire(_Name, [], {_, Acc}) -> Acc;
trace_wire(Name, [{<<"U">>, Int} | T], {{Steps, X, Y}, Acc}) ->
  L = lists:seq(Steps, Steps + Int),
  StepF = fun(I) -> {lists:nth(I, L), X, Y + I} end,
  NewAcc = update_acc(StepF, Int, Name, Acc),
  trace_wire(Name, T, {{Steps + Int, X, Y + Int}, NewAcc});
trace_wire(Name, [{<<"D">>, Int} | T], {{Steps, X, Y}, Acc}) ->
  L = lists:seq(Steps, Steps + Int),
  StepF = fun(I) -> {lists:nth(I, L), X, Y - I} end,
  NewAcc = update_acc(StepF, Int, Name, Acc),
  trace_wire(Name, T, {{Steps + Int, X, Y - Int}, NewAcc});
trace_wire(Name, [{<<"L">>, Int} | T], {{Steps, X, Y}, Acc}) ->
  L = lists:seq(Steps, Steps + Int),
  StepF = fun(I) -> {lists:nth(I, L), X - I, Y} end,
  NewAcc = update_acc(StepF, Int, Name, Acc),
  trace_wire(Name, T, {{Steps + Int, X - Int, Y}, NewAcc});
trace_wire(Name, [{<<"R">>, Int} | T], {{Steps, X, Y}, Acc}) ->
  L = lists:seq(Steps, Steps + Int),
  StepF = fun(I) -> {lists:nth(I, L), X + I, Y} end,
  NewAcc = update_acc(StepF, Int, Name, Acc),
  trace_wire(Name, T, {{Steps + Int, X + Int, Y}, NewAcc}).

update_acc(StepF, Int, Name, Acc) ->
  StepCoords = [StepF(I) || I <- lists:seq(1,Int)],
  lists:foldl(fun(SC, A) -> update(SC, Name, A) end, Acc, StepCoords).

update({Steps, X, Y}, Name, Acc) ->
  maps:update_with({X, Y},
                   fun(V) ->
                       case V of
                         [{_, Name}] -> V;
                         _ -> [{Steps, Name} | V]
                       end
                   end,
                   [{Steps, Name}],
                   Acc).

find_crossed(Out) ->
  lists:sort(
    [ {to_positive(X) + to_positive(Y), V} ||
      {{X,Y}, V} <- maps:to_list(
                      maps:filter(fun(_, V) -> length(V) =:= 2 end, Out)) ]).

to_positive(X) when X < 0 -> X * -1;
to_positive(X) -> X.

%%%_* Input ====================================================================
input() ->
  {ok, Bin} = file:read_file(code:priv_dir(aoc) ++ "/inputs/day3.txt"),
  [Wire1, Wire2] = [ binary:split(Wire, <<",">>, [trim, global])
                     || Wire <- binary:split(Bin, <<"\n">>, [trim, global]) ],
  [parse_input(Wire1), parse_input(Wire2)].

parse_input(WireInput) ->
  [ {D, binary_to_integer(I)} || <<D:1/binary, I/binary>> <- WireInput ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
