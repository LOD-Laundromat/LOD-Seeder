:- module(html_seeder, [run/0]).

/** <module> HTML-based LOD seeder
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(sgml)).

:- use_module(library(http/http_client2)).
:- use_module(library(media_type)).
:- use_module(library(ll/ll_seeder)).
:- use_module(library(sw/rdf_media_type)).
:- use_module(library(uri_ext)).
:- use_module(library(xpath)).





run :-
  run(dbpedia, 'dbpedia-2016-10', 'http://downloads.dbpedia.org/2016-10/').

run(OName, DName, Uri1) :-
  aggregate_all(set(Uri2), html_index_uri(Uri1, Uri2), Uris),
  length(Uris, N),
  writeln(N),
  add_seed(
    _{
      approach: custom_html,
      dataset: _{name: DName, url: Uri1},
      documents: Uris,
      organization: _{name: OName, url: 'http://dbpedia.org'}
    }
  ).



%! html_index_uri(+Uri1:atom, -Uri2:atom) is nondet.

html_index_uri(Uri1, Uri4) :-
  uri_comps(Uri1, uri(Scheme,Authority,Segments1,_,_)),
  http_open2(Uri1, In, [accept(html),metadata(Metas)]),
  Metas = [Meta|_],
  ['text/html'] = Meta.headers.'content-type',
  call_cleanup(
    (
      load_html(In, Dom, []),
      xpath(Dom, //a(@href), Uri2), %NONDET
      \+ memberchk(Uri2, ['../']),
      \+ uri_is_global(Uri2),
      atomic_list_concat(Segments, /, Uri2),
      append_segments(Segments1, Segments, Segments2),
      uri_comps(Uri3, uri(Scheme,Authority,Segments2,_,_))
    ),
    close(In)
  ),
  writeln(Uri3),
  (   is_rdf_uri(Uri3)
  ->  Uri4 = Uri3
  ;   html_index_uri(Uri3, Uri4)
  ).

is_rdf_uri(Uri) :-
  uri_file_extensions(Uri, Exts),
  member(Ext, Exts),
  media_type_extension(MediaType, Ext),
  rdf_media_type(MediaType), !.
