:- module(
  ll_seeder,
  [
    upload_seed/1, % +Seed
    upload_url/1   % +Url
  ]
).

/** <module> LOD Laundromat seeder

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(settings)).

:- use_module(library(conf_ext)).
:- use_module(library(dcg)).
:- use_module(library(http/http_client2)).
:- use_module(library(uri_ext)).

:- initialization
   init_seeder.

:- setting(authority, any, _, "URI scheme of the seedlist server location.").
:- setting(scheme, any, _, "URI scheme of the seedlist server location.").





%! upload_seed(+Seed:dict) is det.
%
% Keys:
%   * description(string)
%   * documents(list(atom))
%   * image(atom)
%   * license(atom)
%   * name(atom)
%   * organization(dict)
%     * name(atom)
%     * image(atom)
%   * url(atom)

upload_seed(Seed) :-
  maplist(setting, [scheme,authority], [Scheme,Auth]),
  uri_comps(Uri, uri(Scheme,Auth,[],_,_)),
  http_open2(Uri, In, [post(json(Seed)),succeed(201)]),
  close(In).



%! upload_url(+Url:atom) is det.

upload_url(Url) :-
  upload_seed(_{documents: [Url], name: Url, url: Url}).





% INITIALIZATION %

init_seeder :-
  conf_json(Conf1),
  _{seedlist: Conf2} :< Conf1,
  _{authority: Authority, scheme: Scheme} :< Conf2,
  maplist(set_setting, [scheme,authority], [Scheme,Authority]).
