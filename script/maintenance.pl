:- module(
  maintenance,
  [
    clear_seedlist/0
  ]
).

/** <module> LOD-Seedlist maintenance script

@author Wouter Beek
@version 2018
*/

:- use_module(library(ll/ll_seeder)).





%! clear_seedlist is det.

clear_seedlist :-
  forall(
    seed(Seed),
    (
      _{hash: Hash} :< Seed,
      delete_seed(Hash)
    )
  ).
