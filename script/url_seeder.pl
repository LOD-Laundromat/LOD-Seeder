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

:- use_module(library(aggregate)).

:- use_module(library(http/http_client2)).
:- use_module(library(ll/ll_seeder)).





%! add_url(+Url:atom) is det.

add_url(Url) :-
  aggregate_all(
    min(LMod),
    (   http_metadata_last_modified(Url, LMod)
    ;   LMod = 0.0
    ),
    LMod
  ),
  add_seed(
    _{dataset: _{'last-modified': LMod, name: Url, url: Url}, documents: [Url]}
  ).
