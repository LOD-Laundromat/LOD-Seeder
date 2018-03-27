:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(lists)).

:- use_module(library(file_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(ll/ll_seeder)).

run :-
  call_stream_file('seeds.json', run_).

run_(In) :-
  json_read_dict(In ,Dicts),
  maplist(run__, Dicts).

run__(Dict) :-
  writeln(Dict),
  aggregate_all(
    min(LMod),
    (   member(Uri, Dict.documents),
        catch(http_metadata_last_modified(Uri, LMod), _, fail)
    ;   LMod = 0.0
    ),
    LMod
  ),
  add_seed(
    _{
      approach: custom,
      dataset: _{'last-modified': LMod, name: Dict.dataset.name},
      documents: Dict.documents,
      organization: Dict.organization
    }
  ).
