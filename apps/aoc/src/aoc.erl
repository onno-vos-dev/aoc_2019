-module(aoc).

-export([ fuel/1
        ]).

-on_load(init/0).

-define(APPNAME, aoc).
-define(LIBNAME, "libaoc").

%%====================================================================
%% API functions - NIFS
%%====================================================================
fuel(A) -> fuel_nif(A).

%%====================================================================
%% NIFS
%%====================================================================
fuel_nif(_) ->
  not_loaded(?LINE).

%%====================================================================
%%%% Internal functions
%%%%%====================================================================
init() ->
  SoName = case code:priv_dir(?APPNAME) of
             {error, bad_name} ->
               case filelib:is_dir(filename:join(["..", priv])) of
                 true ->
                   filename:join(["..", priv, ?LIBNAME]);
                 _ ->
                   filename:join([priv, ?LIBNAME])
               end;
             Dir ->
               filename:join(Dir, ?LIBNAME)
           end,
  erlang:load_nif(SoName, 0).

not_loaded(Line) ->
  exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
