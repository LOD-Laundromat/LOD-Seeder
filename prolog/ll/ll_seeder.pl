:- module(
  ll_seeder,
  [
    add_seed/1,    % +Seed
    add_url/1,     % +Url
    delete_seed/1, % +Hash
    seed/1         % -Seed
  ]
).

/** <module> LOD Laundromat seeder

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(library(conf_ext)).
:- use_module(library(dcg)).
:- use_module(library(http/http_client2)).
:- use_module(library(uri_ext)).

:- initialization
   init_seeder.

:- meta_predicate
    request_(+, +, 1, +).

:- setting(authority, any, _,
           "URI scheme of the seedlist server location.").
:- setting(scheme, oneof([http,https]), https,
           "URI scheme of the seedlist server location.").





%! add_seed(+Seed:dict) is det.
%
% Keys:
%   * dataset(dict)
%     * description(string)
%     * image(atom)
%     * license(atom)
%     * name(atom)
%     * url(atom)
%   * documents(list(atom))
%   * organization(dict)
%     * name(atom)
%     * image(atom)
%     * url(atom)

add_seed(Seed) :-
  catch(request_([seed], _, close, [post(json(Seed)),success(201)]), E, true),
  (   var(E)
  ->  true
  ;   E = error(http_status(200,_),_)
  ->  print_message(informational, seed_already_exists)
  ;   throw(E)
  ).



%! add_url(+Url:atom) is det.

add_url(Url) :-
  add_seed(_{dataset: _{name: Url, url: Url}, documents: [Url]}).



%! delete_seed(+Hash:atom) is det.

delete_seed(Hash) :-
  request_([seed], [hash(Hash)], close, [failure(404),method(delete)]).



%! seed(-Seed:dict) is nondet.

seed(Seed) :-
  request_([seed], _, seed_(Seed), []).

seed_(Seed, In) :-
  call_cleanup(
    (
      json_read_dict(In, Seeds, [value_string_as(atom)]),
      member(Seed, Seeds)
    ),
    close(In)
  ).





% INITIALIZATION %

%! init_seeder is det.

init_seeder :-
  conf_json(Conf1),
  _{seedlist: Conf2} :< Conf1,
  _{authority: Authority, scheme: Scheme} :< Conf2,
  maplist(set_setting, [scheme,authority], [Scheme,Authority]).



%! request_(+Segments:list(atom), ?Query:list(compound), :Goal_1,
%!          +Options:list(compound)) is semidet.

request_(Segments, Query, Goal_1, Options) :-
  maplist(setting, [scheme,authority], [Scheme,Auth]),
  uri_comps(Uri, uri(Scheme,Auth,Segments,Query,_)),
  http_call(Uri, Goal_1, [accept(json)|Options]).
