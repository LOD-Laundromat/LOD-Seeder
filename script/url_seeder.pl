:- module(
  url_seeder,
  [
    add_url/1 % +Url
  ]
).

/** <module> URL seeder

@author Wouter Beek
@version 2018
*/

:- use_module(library(ll/ll_seeder)).





%! add_url(+Url:atom) is det.

add_url(Url) :-
  add_seed(
    _{
      approach: url_seeder,
      dataset: _{'last-modified': 0.0, name: Url, url: Url},
      documents: [Url]
    }
  ).
