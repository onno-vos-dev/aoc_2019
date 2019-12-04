-module(day3).

-compile([export_all]).

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
                   fun(V) -> case V of [{_, Name}] -> V; _ -> [{Steps, Name} | V] end end,
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

part1_tests() ->
  [ {input1, part1(test_input1()) =:= 6}
  , {input2, part1(test_input2()) =:= 159}
  , {input3, part1(test_input3()) =:= 135}
  ].

part2_tests() ->
  [ {input1, part2(test_input1()) =:= 40}
  , {input2, part2(test_input2()) =:= 610}
  , {input3, part2(test_input3()) =:= 410}
  ].

test_input1() ->
  Wire1 = [ <<"R8">>, <<"U5">>, <<"L5">>, <<"D3">>],
  Wire2 = [ <<"U7">>, <<"R6">>, <<"D4">>, <<"L4">>],
  [parse_input(Wire1), parse_input(Wire2)].

test_input2() ->
  Wire1 = [<<"R75">>,<<"D30">>,<<"R83">>,<<"U83">>,<<"L12">>,<<"D49">>,<<"R71">>,<<"U7">>,<<"L72">>],
  Wire2 = [<<"U62">>,<<"R66">>,<<"U55">>,<<"R34">>,<<"D71">>,<<"R55">>,<<"D58">>,<<"R83">>],
    [parse_input(Wire1), parse_input(Wire2)].

test_input3() ->
  Wire1 = [<<"R98">>,<<"U47">>,<<"R26">>,<<"D63">>,<<"R33">>,<<"U87">>,<<"L62">>,<<"D20">>,<<"R33">>,<<"U53">>,<<"R51">>],
  Wire2 = [<<"U98">>,<<"R91">>,<<"D20">>,<<"R16">>,<<"D67">>,<<"R40">>,<<"U7">>,<<"R15">>,<<"U6">>,<<"R7">>],
  [parse_input(Wire1), parse_input(Wire2)].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
