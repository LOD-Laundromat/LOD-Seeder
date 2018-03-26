:- module(
  void_seeder,
  [
    run/0
  ]
).

/** <module> VoID seeder

@author Wouter Beek
@version 2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(solution_sequences)).

:- use_module(library(http/http_client2)).
:- use_module(library(ll/ll_seeder)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir)).

:- maplist(rdf_assert_prefix, [
     dct-'http://purl.org/dc/terms/',
     foaf-'http://xmlns.com/foaf/0.1/',
     rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
     rdfs-'http://www.w3.org/2000/01/rdf-schema#',
     void-'http://rdfs.org/ns/void#'
   ]).





run :-
  dataset_metadata(OName, DName, Triples),
  maplist(writeln, Triples),
  seed_documents(Triples, Docs),
  Docs \== [],
  seed_dataset(Triples, Docs, Dataset),
  seed_organization(OName, DName, Triples, Org),
  add_seed(_{dataset: Dataset, documents: Docs, organization: Org}).



%! dataset_metadata(-OName:atom, -DName:atom,
%!                  -Triples:ordset(rdf_triple)) is nondet.

% void:Dataset, void:Linkset
dataset_metadata(OName, DName, Triples) :-
  distinct([OName,DName,S], dataset_(OName, DName, S)),
  findall(Triple, cbd(OName, DName, S, Triple), Triples).

dataset_(OName, DName, S) :-
  dataset(OName, DName, _),
  (   % Instance of a VoID class.
      rdf_prefix_member(C, [void:'Dataset',void:'Linkset']),
      statement(OName, DName, S, rdf:type, C)
  ;   % Domain of a VoID property.
      rdf_prefix_member(
        P,
        [void:dataDump,void:documents,void:sparqlEndpoint,void:subset]
      ),
      statement(OName, DName, S, P, _)
  ;   % Range of a VoID property.
      statement(OName, DName, _, void:subset, S)
  ).



%! seed_dataset(+Triples:ordset(rdf_triple), +Docs:ordset(atom),
%!              -Dataset:dict) is det.

seed_dataset(Triples, Docs, Dict4) :-
  seed_last_modified(Triples, Docs, LMod),
  seed_dataset_name_(Triples, Name),
  Dict1 = _{'last-modified': LMod, name: Name},
  seed_dataset_description_(Triples, Dict1, Dict2),
  seed_dataset_license_(Triples, Dict2, Dict3),
  seed_dataset_url_(Triples, Dict3, Dict4).

% dct:description
seed_dataset_description_(Triples, Dict1, Dict2) :-
  rdf_prefix_member(rdf(_,dct:description,Desc0), Triples),
  rdf_literal_lexical_form(Desc0, Desc), !,
  Dict2 = Dict1.put(_{description: Desc}).
seed_dataset_description_(_, Dict, Dict).

% dct:license, wv:norms, wv:waiver
seed_dataset_license_(Triples, Dict1, Dict2) :-
  rdf_prefix_member(P, [dct:license,wv:norms,wv:waiver]),
  rdf_prefix_member(rdf(_,P,License), Triples), !,
  writeln(license-License),%DEB
  Dict2 = Dict1.put(_{license: License}).
seed_dataset_license_(_, Dict, Dict).

% dct:title, rdfs:label
seed_dataset_name_(Triples, Name) :-
  rdf_prefix_member(P, [dct:title,rdfs:label]),
  rdf_prefix_member(rdf(_,P,Name0), Triples),
  rdf_literal_lexical_form(Name0, Name), !.

% foaf:homepage, foaf:page, dct:source, foaf:mbox
seed_dataset_url_(Triples, Dict1, Dict2) :-
  rdf_prefix_member(P, [foaf:homepage,foaf:page,dct:source,foaf:mbox]),
  rdf_prefix_member(rdf(_,P,Url), Triples), !,
  writeln(url-Url),%DEB
  Dict2 = Dict1.put(_{url: Url}).
seed_dataset_url_(_, Dict, Dict).



%! seed_documents(+Triples:ordset(rdf_triple), -Urls:ordset(atom)) is det.

% void:dataDump
seed_documents(Triples, Urls) :-
  aggregate_all(
    set(Url),
    rdf_prefix_member(rdf(_,void:dataDump,Url), Triples),
    Urls
  ).



%! seed_last_modified(+Triples:ordset(rdf_triple), +Urls:ordset(atom),
%!                    -LMod:float) is det.

seed_last_modified(Triples, Urls, LMod) :-
  aggregate_all(
    min(LMod),
    (   seed_last_modified_(Triples, LMod)
    ;   member(Url, Urls),
        http_metadata_last_modified(Url, LMod)
    ;   LMod = 0.0
    ),
    LMod
  ).

% dct:modified, dct:issues, dct:created, dct:date
seed_last_modified_(Triples, LMod) :-
  rdf_prefix_member(P, [dct:modified,dct:issued,dct:created,dct:date]),
  rdf_prefix_member(rdf(_,P,LMod0), Triples),
  rdf_literal_value(LMod0, LMod), !,
  writeln(LMod).



%! seed_organization(+OName:atom, +DName:atom, +Triples:ordset(rdf_triple),
%!                   -Organization:dict) is det.

% dct:publisher, dct:creator, dct:contributor
seed_organization(OName, DName, Triples, Dict2) :-
  rdf_prefix_member(P, [dct:publisher,dct:creator,dct:contributor]),
  rdf_prefix_member(rdf(_,P,Org0), Triples),
  (   rdf_literal_lexical_form(Org0, Name)
  ->  Dict2 = _{name: Name}
  ;   statement(OName, DName, Org0, rdfs:label, Name),
      Dict1 = _{name: Name},
      (   rdf_prefix_member(Q, [foaf:homepage,foaf:page,dct:source,foaf:mbox]),
          statement(OName, DName, Org0, Q, Url)
      ->  Dict2 = Dict1.put(_{url: Url})
      ;   Dict2 = Dict1
      )
  ).
