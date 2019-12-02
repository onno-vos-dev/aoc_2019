-module(day2).

-export([ part1/0
        , part1/1
        , part2/0
        ]).

part1() ->
  part1(input()).

part1(Input0) ->
  Input = reset_memory(Input0, 12, 2),
  Res = process_ops(Input, 0),
  {Res, Res =:= 8017076}.

part2() ->
  L = lists:seq(1,100),
  Resets = [{A,B} || A <- L, B <- L],
  Res = do_part2(input(), Resets),
  {Res, Res =:= 3146}.

do_part2(Input0, [{Noun, Verb}|T]) ->
  try
    Input = reset_memory(Input0, Noun, Verb),
    case process_ops(Input, 0) of
      19690720 -> 100 * Noun + Verb;
      _ ->
        do_part2(Input0, T)
    end
  catch _:_:_ ->
      do_part2(Input0, T)
  end.

%%%_* Internal =================================================================
reset_memory(Input0, Noun, Verb) ->
  Input1 = maps:put(1, Noun, Input0),
  maps:put(2, Verb, Input1).

process_ops(Acc, Ops) ->
  case maps:get(Ops, Acc) of
    99 -> maps:get(0, Acc);
    Op ->
      PosA = maps:get(Ops + 1, Acc),
      PosB = maps:get(Ops + 2, Acc),
      PosC = maps:get(Ops + 3, Acc),
      process_ops(do_op(Op, PosA, PosB, PosC, Acc), Ops + 4)
  end.

do_op(1, IntA, IntB, Pos, Map) ->
  maps:put(Pos, maps:get(IntA, Map) + maps:get(IntB, Map), Map);
do_op(2, IntA, IntB, Pos, Map) ->
  maps:put(Pos, maps:get(IntA, Map) * maps:get(IntB, Map), Map);
do_op(99, _IntA, _IntB, _Pos, Map) -> Map.

%%%_* Input ====================================================================
input() ->
  maps:from_list(
    tagged_input(
      util:read_file("day2.txt", <<",">>, fun(X) -> binary_to_integer(X) end))).

tagged_input(Input) ->
  lists:zip(lists:seq(0, length(Input) - 1), Input).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
