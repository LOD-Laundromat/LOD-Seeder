:- module(
  ll_seeder,
  [
    add_seed/1, % +Seed
    add_url/1,  % +Url
    seed/1      % -Seed
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
  maplist(setting, [scheme,authority], [Scheme,Auth]),
  uri_comps(Uri, uri(Scheme,Auth,[seed],_,_)),
  http_open2(Uri, In, [post(json(Seed)),success(201)]),
  close(In).



%! add_url(+Url:atom) is det.

add_url(Url) :-
  add_seed(_{dataset: _{name: Url, url: Url}, documents: [Url]}).



%! seed(-Seed:dict) is nondet.

seed(Seed) :-
  maplist(setting, [scheme,authority], [Scheme,Auth]),
  uri_comps(Uri, uri(Scheme,Auth,[seed],_,_)),
  http_open2(Uri, In, [accept(json)]),
  call_cleanup(
    (
      json_read_dict(In, Seeds),
      member(Seed, Seeds)
    ),
    close(In)
  ).





% INITIALIZATION %

init_seeder :-
  conf_json(Conf1),
  _{seedlist: Conf2} :< Conf1,
  _{authority: Authority, scheme: Scheme} :< Conf2,
  maplist(set_setting, [scheme,authority], [Scheme,Authority]).
