:- module(
  ckan_seeder,
  [
    ckan_scrape_site/1,  % ?Site
    ckan_scrape_sites/0
  ]
).

/** <module> CKAN seeder

@author Wouter Beek
@version 2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(yall)).

:- use_module(library(atom_ext)).
:- use_module(library(counter)).
:- use_module(library(dcg)).
:- use_module(library(http/ckan_api)).
:- use_module(library(ll/ll_seeder)).
:- use_module(library(media_type)).
:- use_module(library(thread_ext)).

:- dynamic
    ckan_site__/1.





%! ckan_add_seed(+Site:atom, +LMod:float, +Package:dict) is det.

ckan_add_seed(Site, LMod, Package) :-
  _{name: DName, resources: Resources} :< Package,
  % .dataset.description
  Dataset1 = _{'last-modified': LMod, name: DName},
  ckan_description_(Package, Dataset1, Dataset2),
  % .dataset.license
  ckan_license_(Package, Dataset2, Dataset3),
  % .documents
  maplist(ckan_resource_url_, Resources, Docs),
  % .organization
  Org1 = _{url: Site},
  ckan_organization_(Package, Org1, Org2),
  add_seed(_{dataset: Dataset3, documents: Docs, organization: Org2}).

ckan_description_(Package, Dataset1, Dataset2) :-
  _{notes: Desc0} :< Package,
  Desc0 \== null, !,
  atom_string(Desc0, Desc),
  Dataset2 = Dataset1.put(_{description: Desc}).
ckan_description_(_, Dataset, Dataset).

ckan_license_(Package, Dataset1, Dataset2) :-
  _{license_url: License} :< Package, !,
  Dataset2 = Dataset1.put(_{license: License}).
ckan_license_(_, Dataset, Dataset).

ckan_organization_(Package, Org1, Org2) :-
  is_dict(Package.organization),
  % name
  _{name: OName} :< Package.organization, !,
  % image
  (   _{image_url: Url} :< Package.organization
  ->  Org2 = Org1.put(_{image: Url, name: OName})
  ;   Org2 = Org1.put(_{name: OName})
  ).
ckan_organization_(_, Org, Org).

ckan_resource_url_(Resource, Url) :-
  _{url: Url} :< Resource.



%! ckan_package_last_modified(+Package:dict, -LMod:float) is det.

ckan_package_last_modified(Package, LMod) :-
  _{resources: Resources} :< Package,
  aggregate_all(
    min(LMod),
    (   member(Resource, Resources),
        ckan_resource_last_modified(Resource, LMod)
    ;   LMod = 0.0
    ),
    LMod
  ).



%! ckan_package_media_types(+Site:atom, +Package:dict, -Resource:dict,
%!                          -MediaTypes:list(compound)) is multi.

ckan_package_media_types(Site, Package, Resource, MediaTypes) :-
  _{resources: Resources} :< Package,
  aggregate_all(
    set(MediaType),
    (
      member(Resource, Resources),
      _{format: Format1, mimetype: Format2} :< Resource,
      member(Format, [Format1,Format2]),
      clean_media_type(Site, Format, MediaType)
    ),
    MediaTypes
  ).



%! ckan_print_report(+Site:atom) is det.
%! ckan_print_report(-Site:atom) is nondet.

ckan_print_report(Site) :-
  ckan_site__(Site),
  setting(ll_seeder:data_directory, Dir),
  atom_phrase(file_name_, Site, Name),
  directory_file_path(Dir, Name, Base),
  file_name_extension(Base, pl, File),
  setup_call_cleanup(
    open(File, write, Out),
    (
      % unknown formats
      findall(
        N-Format,
        counter(ckan_unknown_format__(Site,Format), N),
        Pairs1
      ),
      forall(
        member(N-Format, Pairs1),
        format(Out, "~W\n", [format(Format,N),[quoted(true)]])
      ),
      retractall_counter(ckan_unknown_format__(Site,_)),
      % unknown Media Types
      findall(
        N-MediaType,
        counter(ckan_unknown_media_type__(Site,MediaType), N),
        Pairs2
      ),
      forall(
        member(N-MediaType, Pairs2),
        format(Out, "~W\n", [media_type(MediaType,N),[quoted(true)]])
      ),
      retractall_counter(ckan_unknown_media_type__(Site,_))
    ),
    close(Out)
  ),
  retractall(ckan_site__(Site)).

file_name_, [Code] -->
  [Code],
  {code_type(Code, alnum)}, !,
  file_name_.
file_name_, "-" -->
  [_], !,
  file_name_.
file_name_--> "".



%! ckan_scrape_package(+Site:atom, -LMod:float, -Package:dict) is nondet.

ckan_scrape_package(Site, LMod, Package) :-
  ckan_package(Site, Package),
  (   % The dataset contains at least one RDF document.
      ckan_package_media_types(Site, Package, _, MediaTypes),
      member(MediaType, MediaTypes),
      rdf_media_type_(MediaType)
  ->  ckan_package_last_modified(Package, LMod),
      format("✓"),
      flush_output(user_output)
  ;   format("❌"),
      flush_output(user_output),
      fail
  ).



%! ckan_resource_last_modified(+Resource:dict, -LMod:float) is semidet.

ckan_resource_last_modified(Resource, LMod) :-
  catch(ckan_resource_last_modified_(Resource, LMod), _, fail).

ckan_resource_last_modified_(Resource, LMod) :-
  get_dict(last_modified, Resource, Str),
  Str \== null,
  parse_time(Str, iso_8601, LMod), !.
ckan_resource_last_modified_(Resource, LMod) :-
  _{created: Str} :< Resource,
  parse_time(Str, iso_8601, LMod), !.




%! ckan_scrape_site(+Site:atom) is det.
%! ckan_scrape_site(-Site:atom) is nondet.

ckan_scrape_site(Site) :-
  % Ensure that Site is instantiated.
  (var(Site) -> ckan_site_uri(Site) ; true),
  % Store the visited CKAN sites locally for report printing.
  (ckan_site__(Site) -> true ; assertz(ckan_site__(Site))),
  thread_create(ckan_scrape_site_(Site), Id, [alias(Site)]),
  thread_join(Id, Status),
  (   Status == true
  ->  true
  ;   print_message(warning, ckan_scrape_site(Site,Status))
  ),
  ckan_print_report(Site).

ckan_scrape_site_(Site) :-
  forall(
    ckan_scrape_package(Site, LMod, Package),
    ckan_add_seed(Site, LMod, Package)
  ).

rdf_media_type_(media(application/'n-quads',[])).
rdf_media_type_(media(application/'n-triples',[])).
rdf_media_type_(media(application/'rdf+xml',[])).
rdf_media_type_(media(application/trig,[])).
rdf_media_type_(media(text/turtle,[])).



%! ckan_scrape_sites is det.

ckan_scrape_sites :-
  % Store output in log file.
  setting(ll_seeder:data_directory, Dir),
  directory_file_path(Dir, 'out.log', File),
  protocol(File),
  % Scrape all known CKAN sites using the given number of threads.
  forall(
    ckan_site_uri(Site),
    create_detached_thread(ckan_scrape_site(Site))
  ).





% CLEANUP CODE %

%! clean_media_type(+Site:atom, +Format:atom, -MediaType:compound) is semidet.

clean_media_type(Site, Format1, MediaType) :-
  downcase_atom(Format1, Format2),
  atom_strip(Format2, Format3),
  \+ memberchk(Format3, ['',null]),
  (   atom_phrase(media_type(MediaType0), Format3)
  ->  (   % The CKAN supplied Media Type can be mapped onto a known
          % Media Type.
          ckan_known_media_type(MediaType0, MediaType)
      ->  true
      ;   % The CKAN supplied Media Type cannot be mapped onto a known
          % Media Type.
          increment_counter(ckan_unknown_media_type__(Site,Format3)),
          fail
      )
  ;   ckan_known_format(Format3, MediaType)
  ->  true
  ;   increment_counter(ckan_unknown_format__(Site,Format3)),
      fail
  ).

% The CKAN format is the default file extension of a registered Media
% Type.
ckan_known_format(Format, MediaType) :-
  media_type_extension(MediaType, Format).
ckan_known_format(Format1, MediaType) :-
  % Remove the leading dot, if any.
  (   sub_atom(Format1, 0, 1, _, .)
  ->  sub_atom(Format1, 1, _, 0, Format2)
  ;   Format2 = Format1
  ),
  % Remove the compression file extension, if any.
  (   file_name_extension(Format3, zip, Format2)
  ->  true
  ;   Format3 = Format2
  ),
  ckan_format_(Format3, MediaType).

% Registered Media Type.
ckan_known_media_type(media(Super/Sub,Params), media(Super/Sub,Params)) :-
  media_type(media(Super/Sub,_)).
% Can be mapped onto a registered Media Type.
ckan_known_media_type(MediaType1, MediaType2) :-
  ckan_media_type_(MediaType1, MediaType2).

% Map typo's and alternative names onto registered Media Types.
ckan_format_('7z',                        media(application/'x-7z-compressed',[])).
ckan_format_('atom feed',                 media(application/'atom+x',[])).
ckan_format_(biopax,                      media(application/'vnd.biopax.rdf+xml',[])).
ckan_format_('bz2:nt',                    media(application/'n-triples',[])).
ckan_format_('bz2:xml',                   media(application/xml,[])).
ckan_format_(cvs,                         media(text/csv,[])).
ckan_format_('data file in excel',        media(application/'vnd.ms-excel',[])).
ckan_format_(excel,                       media(application/'vnd.ms-excel',[])).
% JSON-stat
ckan_format_('json-stat',                 media(application/json,[])).
ckan_format_('geo.json',                  media(application/'vnd.geo+json',[])).
ckan_format_(geojsom,                     media(application/'vnd.geo+json',[])).
ckan_format_(georss,                      media(application/'rss+xml',[])).
ckan_format_(geotiff,                     media(image/tiff,[])).
ckan_format_(gzip,                        media(application/gzip,[])).
ckan_format_('gzip::nquads',              media(application/'n-quads',[])).
ckan_format_('gzip:ntriples',             media(application/'n-triples',[])).
ckan_format_(htm,                         media(text/html,[])).
ckan_format_('html+rdfa',                 media(application/'xhtml+xml',[])).
ckan_format_(html5,                       media(text/html,[])).
% International Aid Transparency Initiative (IATI) XML
ckan_format_('iati-xml',                  media(text/xml,[])).
ckan_format_(jpg,                         media(image/jpeg,[])).
ckan_format_('json-ld',                   media(application/'ld+json',[])).
ckan_format_('microsoft access',          media(application/'vnd.ms-access',[])).
ckan_format_('microsoft access database', media(application/'vnd.ms-access',[])).
ckan_format_('microsoft excel',           media(application/'vnd.ms-excel',[])).
ckan_format_(mol,                         media(chemical/'x-mdl-molfile',[])).
ckan_format_('ms access',                 media(application/'vnd.ms-access',[])).
ckan_format_('ms access mdb',             media(application/'vnd.ms-access',[])).
ckan_format_('n-quads',                   media(application/'n-quads',[])).
ckan_format_('n-triples',                 media(application/'n-triples',[])).
ckan_format_(netcdf,                      media(application/netcdf,[])).
ckan_format_('ods format',                media(application/'vnd.oasis.opendocument.spreadsheet',[])).
ckan_format_('rdf trig',                  media(application/trig,[])).
ckan_format_('rdf+xml',                   media(application/'rdf+xml',[])).
ckan_format_('rdf-n3',                    media(text/n3,[])).
ckan_format_('rdf-turtle',                media(text/turtle,[])).
ckan_format_('rdf-xml',                   media(application/'rdf+xml',[])).
ckan_format_(rdfa,                        media(application/'xhtml+xml',[])).
ckan_format_(rdfxml,                      media(application/'rdf+xml',[])).
ckan_format_('rss + xml',                 media(application/'rss+xml',[])).
ckan_format_('rss+xml',                   media(application/'rss+xml',[])).
ckan_format_('sparql-query',              media(application/'sparql-query',[])).
ckan_format_('sparql-json',               media(application/'sparql-results+json',[])).
ckan_format_('tab-separated-values',      media(text/'tab-separated-values',[])).
ckan_format_('tar.gz',                    media(application/'x-tar',[])).
ckan_format_(tif,                         media(image/tiff,[])).
ckan_format_('trig gzip',                 media(application/trig,[])).
ckan_format_('ttl.bz2',                   media(text/turtle,[])).
ckan_format_('ttl.bzip2',                 media(text/turtle,[])).
ckan_format_('xls (zip)',                 media(application/'vnd.ms-excel',[])).
ckan_format_('xml (zipped)',              media(text/xml,[])).
ckan_format_('zip archive',               media(application/zip,[])).
ckan_format_('zip:csv',                   media(text/csv,[])).
ckan_format_('sparql-xml',                media(application/'sparql-results+xml',[])).
ckan_format_(tab,                         media(text/'tab-separated-values',[])).
ckan_format_(tar,                         media(application/'x-tar',[])).
ckan_format_(tgz,                         media(application/'x-tar',[])).
% @tbd Generalize: `xxx.gz' has the Media Type of `xxx'.
ckan_format_('tsv.gz',                    media(text/'tab-separated-values',[])).
ckan_format_(turtle,                      media(text/turtle,[])).
ckan_format_(xlxs,                        media(application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet',[])).
ckan_format_(xsl,                         media(application/'vnd.ms-excel',[])).
ckan_format_(xslx,                        media(application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet',[])).
ckan_format_(yaml,                        media(application/'x-yaml',[])).

ckan_media_type_(media(application/csv,L),               media(text/csv,L)).
ckan_media_type_(media(application/'gpx-xml',L),         media(application/'gpx+xml',L)).
ckan_media_type_(media(application/msexcel,L),           media(application/'vnd.ms-excel',L)).
ckan_media_type_(media(application/rar,L),               media(application/'vnd.rar',L)).
ckan_media_type_(media(application/'rdf xml',L),         media(application/'rdf+xml',L)).
ckan_media_type_(media(application/'vnd.openxmlformates-officedocument.spreadsheetml.sheet',L), media(application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet',L)).
ckan_media_type_(media(application/'x-bzip',L),          media(application/'x-bzip2',L)).
ckan_media_type_(media(application/'x-bzip2',L),         media(application/'x-bzip2',L)).
ckan_media_type_(media(application/'x-gzip',L),          media(application/gzip,L)).
ckan_media_type_(media(application/'x-netcdf',L),        media(application/netcdf,L)).
ckan_media_type_(media(application/'x-nquads',L),        media(application/'n-quads',L)).
ckan_media_type_(media(application/'x-ntriples',L),      media(application/'n-triples',L)).
ckan_media_type_(media(application/'x-pdf',L),           media(application/pdf,L)).
ckan_media_type_(media(application/'x-tgz',L),           media(application/'x-tar',L)).
ckan_media_type_(media(application/'x-trig',L),          media(application/trig,L)).
ckan_media_type_(media(application/'x-turtle',L),        media(text/turtle,L)).
ckan_media_type_(media(application/'x-vnd.oasis.opendocument.spreadsheet',[]), media(application/'vnd.oasis.opendocument.spreadsheet',[])).
ckan_media_type_(media(application/xml,L),               media(text/xml,L)).
ckan_media_type_(media(application/'xml+atom',L),        media(application/'atom+x',L)).
ckan_media_type_(media(application/'xml+rdf',L),         media(application/'rdf+xml',L)).
ckan_media_type_(media(example/'application/rdf+xml',L), media(application/'rdf+xml',L)).
ckan_media_type_(media(example/'html+rdfa',L),           media(application/'xhtml+xml',L)).
ckan_media_type_(media(example/'rdf xml',L),             media(application/'rdf+xml',L)).
ckan_media_type_(media(example/'rdf+json',L),            media(application/'ld+json',L)).
ckan_media_type_(media(example/'rdf+ttl',L),             media(text/turtle,L)).
ckan_media_type_(media(example/'rdf+xml',L),             media(application/'rdf+xml',L)).
ckan_media_type_(media(example/'x-turtle',L),            media(text/turtle,L)).
ckan_media_type_(media(example/n3,L),                    media(text/n3,L)).
ckan_media_type_(media(example/ntriples,L),              media(application/'n-triples',L)).
ckan_media_type_(media(example/rdfa,L),                  media(application/'xhtml+xml',L)).
ckan_media_type_(media(example/turtle,L),                media(text/turtle,L)).
ckan_media_type_(media(html/doc,L),                      media(text/html,L)).
ckan_media_type_(media(html/rdf,L),                      media(application/'xhtml_xml',L)).
ckan_media_type_(media(kml/kmz,L),                       media(application/'vnd.google-earth.kml+xml',L)).
ckan_media_type_(media(marc/xml,L),                      media(text/xml,L)).
ckan_media_type_(media(rdf/'n-triples',L),               media(application/'n-triples',L)).
ckan_media_type_(media(rdf/'xml example',L),             media(application/'rdf+xml',L)).
ckan_media_type_(media(rdf/n3,L),                        media(text/n3,L)).
ckan_media_type_(media(rdf/nt,L),                        media(application/'n-triples',L)).
ckan_media_type_(media(rdf/turtle,L),                    media(text/turtle,L)).
ckan_media_type_(media(sparql/json,L),                   media(application/'sparql-results+json',L)).
ckan_media_type_(media(sparql/xml,L),                    media(application/'sparql-results+xml',L)).
ckan_media_type_(media(text/'comma-separated-values',L), media(text/csv,L)).
ckan_media_type_(media(text/'rdf+n3',L),                 media(text/n3,L)).
ckan_media_type_(media(text/'rdf+ttl',L),                media(text/turtle,L)).
ckan_media_type_(media(text/'x-csv',L),                  media(text/csv,L)).
ckan_media_type_(media(text/javascript,L),               media(application/javascript,L)).
ckan_media_type_(media(text/n3,L),                       media(text/n3,L)).
ckan_media_type_(media(text/sql,L),                      media(application/sql,L)).
ckan_media_type_(media(wfs/gml,L),                       media(application/'gml+xml',L)).
ckan_media_type_(media(xml/rdf,L),                       media(application/'rdf+xml',L)).
ckan_media_type_(media(zip/json,L),                      media(application/json,L)).
ckan_media_type_(media(zip/tsv,L),                       media(text/'tab-separated-values',L)).
