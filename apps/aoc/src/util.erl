-module(util).

-export([ read_file/1
        , time_avg/2
        ]).

read_file(File) ->
  file:read_file(code:priv_dir(aoc) ++ "/inputs/" ++ File).

time_avg(Fun, X) ->
  AvgTimeMicro = lists:sum(
                   lists:map(fun(_) ->
                                 {Avg, _} = timer:tc(fun() -> Fun() end),
                                 Avg
                             end, lists:seq(1, X))) / X,
  io:format("=== Time: ~.5f ms ~n~n", [AvgTimeMicro / 1000]).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
