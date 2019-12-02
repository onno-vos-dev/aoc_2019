-module(day2).

-export([ part1/1
        , part2/1
        , input/0
        , test_input/0
        , tagged_input/1
        , op/2
        ]).

part1(Input0) ->
  Input1 = lists:keyreplace(1, 1, Input0, {1,12}),
  Input = lists:keyreplace(2, 1, Input1, {2,2}),
  {_, Res} = lists:unzip(do_part1(Input, {4, Input})),
  hd(Res).

do_part1(L, {_, Acc}) when length(L) < 4 -> Acc;
do_part1([A,B,C,D|_], {Ops, Acc}) ->
  NewAcc = op([A,B,C,D], Acc),
  T = lists:dropwhile(fun({X, _}) ->
                          X < Ops
                      end, NewAcc),
  %% io:format("Ops: ~p~n NewAcc: ~p~n ABCD: ~p~n T: ~p~n~n", [Ops, NewAcc, {A,B,C,D}, T]),
  do_part1(T, {Ops + 4, NewAcc}).

part2(Input) ->
  do_part2(Input).

do_part2(Input) ->
  try
    Res = do_do_part2(Input, rand:uniform(100), rand:uniform(100)),
    case Res of
      {19690720, Noun, Verb} -> {{Noun, Verb}, 100 * Noun + Verb};
      _ -> do_part2(Input)
    end
  catch
    _:_:_ ->
      do_part2(Input)
  end.

do_do_part2(Input0, Noun, Verb) ->
  Input1 = lists:keyreplace(1, 1, Input0, {1, Noun}),
  Input = lists:keyreplace(2, 1, Input1, {2, Verb}),
  {_, Res} = lists:unzip(do_part1(Input, {4, Input})),
  {hd(Res), Noun, Verb}.

op([{_,1}, {_,Pos1}, {_,Pos2}, {_,StorePos}], List) ->
  %% io:format("P1: ~p P1: ~p P3: ~p~n~n", [P1, P2, P3]),
  {_, A} = lists:keyfind(Pos1, 1, List),
  {_, B} = lists:keyfind(Pos2, 1, List),
  lists:keyreplace(StorePos, 1, List, {StorePos, A+B});
op([{_,2}, {_,Pos1}, {_,Pos2}, {_,StorePos}], List) ->
  %% io:format("P1: ~p P1: ~p P3: ~p~n~n", [P1, P2, P3]),
  {_, A} = lists:keyfind(Pos1, 1, List),
  {_, B} = lists:keyfind(Pos2, 1, List),
  lists:keyreplace(StorePos, 1, List, {StorePos, A*B});
op([{_,99}, _, _, _], List) -> List.


input() ->
  util:read_file("day2.txt", <<",">>, fun(X) -> binary_to_integer(X) end).

tagged_input(Input) ->
  lists:zip(lists:seq(0, length(Input) - 1), Input).

test_input() ->
  [1,9,10,3,2,3,11,0,99,30,40,50].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
