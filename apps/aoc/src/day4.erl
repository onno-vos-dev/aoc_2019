-module(day4).

-export([run/0]).

-define(i2l(I), integer_to_list(I)).
-define(l2i(I), list_to_integer(I)).
-define(REGEX, "(\\d)\\1+").

run() ->
  Input = [ integer_to_list(I) || I <- lists:seq(145852, 616942)],
  run(Input, rules_part1(), rules_part2()).

run(Input, Rules1, Rules2) ->
  lists:foldl(
    fun(I, {P1,P2}) ->
        case {validate(I, Rules1), validate(I, Rules2)} of
          {true, true} -> {P1 + 1, P2 + 1};
          {false, false} -> {P1,P2};
          {true, false} -> {P1 + 1, P2};
          {false, true} -> {P1, P2 + 1}
        end
    end, {0, 0}, Input).

validate(I, Rules) ->
  lists:all(fun(RuleF) -> RuleF(I) end, Rules).

%%%_* Internal =================================================================
rules_part1() ->
  {ok, MP} = re:compile(?REGEX),
  [ fun(I) -> lists:sort(I) =:= I end
  , fun(I) -> re:run(I, MP, [global, {capture, none}]) =:= match end
  ].

rules_part2() ->
  {ok, MP} = re:compile(?REGEX),
  [ fun(I) -> lists:sort(I) =:= I end
  , fun(I) ->
        case re:run(I, MP, [global, {capture, first, list}]) of
          nomatch -> false;
          {match, Results} ->
            lists:any(fun([R]) -> length(R) =:= 2 end, Results)
        end
    end
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
