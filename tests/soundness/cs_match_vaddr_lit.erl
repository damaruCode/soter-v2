-module(cs_match_vaddr_lit).

-compile(export_all).

main() ->
  S = 'a',
  case S of
    'a' -> 'b';
    X -> X
  end.
