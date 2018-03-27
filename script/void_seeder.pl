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
  dataset_metadata(OName, DName, Node, Triples),
  maplist(writeln, Triples),
  run_(OName, DName, Node, Triples).

run_(OName, DName, Node, Triples) :-
  seed_documents(Triples, Docs),
  Docs \== [],
  seed_dataset(Triples, Dataset),
  Seed1 = _{approach: Approach, dataset: Dataset, documents: Docs},
  (   seed_organization(OName, DName, Triples, Org)
  ->  Seed2 = Seed1.put(_{organization: Org})
  ;   Seed2 = Seed1
  ),
  format(atom(Approach), "VoID ~a/~a ~a", [OName,DName,Node]),
  %add_seed(Seed2),
  writeln(Seed2), !.
run_(OName, DName, Node, Triples) :-
  gtrace,
  run_(OName, DName, Node, Triples).



%! dataset_metadata(-OName:atom, -DName:atom, -Node:rdf_nonliteral,
%!                  -Triples:ordset(rdf_triple)) is nondet.

% void:Dataset, void:Linkset
dataset_metadata(OName, DName, Node, Triples) :-
  distinct([OName,DName,Node], dataset_node(OName, DName, Node)),
  findall(Triple, cbd(OName, DName, Node, Triple), Triples).



%! dataset_node(-OName:atom, -DName:atom, -Node:rdf_nonliteral) is nondet.

dataset_node(OName, DName, Node) :-
  dataset(OName, DName, _),
  (   % Instance of a VoID class.
      rdf_prefix_member(C, [void:'Dataset',void:'Linkset']),
      statement(OName, DName, Node, rdf:type, C)
  ;   % Domain of a VoID property.
      rdf_prefix_member(
        P,
        [void:dataDump,void:documents,void:sparqlEndpoint,void:subset]
      ),
      statement(OName, DName, Node, P, _)
  ;   % Range of a VoID property.
      statement(OName, DName, _, void:subset, Node)
  ).



%! seed_dataset(+Triples:ordset(rdf_triple), -Dataset:dict) is det.

seed_dataset(Triples, Dict4) :-
  seed_last_modified(Triples, LMod),
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



%! seed_last_modified(+Triples:ordset(rdf_triple), -LMod:float) is det.

seed_last_modified(Triples, LMod) :-
  aggregate_all(
    min(LMod),
    (   rdf_prefix_member(P, [dct:modified,dct:issued,dct:created,dct:date]),
        rdf_prefix_member(rdf(_,P,Literal), Triples),
        rdf_literal_value(Literal, Xsd),
        gtrace,
        date_time_(Xsd, Pl),
        date_time_stamp(Pl, LMod)
    ;   LMod = 0.0
    ),
    LMod
  ).

date_time_(date_time(Y,Mo,D,H,Mi,S), date(Y,Mo,D,H,Mi,S)).



%! seed_organization(+OName:atom, +DName:atom, +Triples:ordset(rdf_triple),
%!                   -Organization:dict) is semidet.

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
  ), !.
