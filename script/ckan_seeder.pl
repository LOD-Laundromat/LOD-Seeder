:- module(
  ckan_seeder,
  [
    ckan_scrape_site/1,  % ?Site
    ckan_scrape_sites/1  % +NumThreads
  ]
).

/** <module> CKAN seeder

% Access ACCDB [Microsoft]
ckan_format_(accdb).
ckan_format_(adf).
% Application Programming Interface (API).
ckan_format_(api).
% ArcGIS [ESRI]
ckan_format_(arcgis).
% ASCII
ckan_format_(ascii).
% ASP.NET [Microsoft]
ckan_format_(asp).
% ASP.NET [Microsoft]
ckan_format_(aspx).
ckan_format_(bin).
% AutoCAD
ckan_format_(autocad).
% Biological Pathway Exchange (BioPAX) [RDF/OWL-based]
ckan_format_(biopax).
ckan_format_(creole).
ckan_format_(dat).
% Common file extension of dBase database files.
% ShapeFile-related [ESRI]
ckan_format_(dbf).
% DGN (design) is the name used for CAD file formats supported by
% Bentley Systems, MicroStation and Intergraph's Interactive Graphics
% Design System (IGDS) CAD programs.
ckan_format_(dgn).
ckan_format_(dia).
ckan_format_(document).
% Document Type Definition (DTD)
ckan_format_(dtd).
% ArcInfo interchange file (E00) [ESRI]
ckan_format_(e00).
ckan_format_(emme).
% executable
ckan_format_(exe).
% File Geodatabase (FileGDB) [ESRI]
ckan_format_(fgdb).
ckan_format_(file).
% File Transfer Protocol (FTP)
ckan_format_(ftp).
ckan_format_(gdb).
% TerraGo GeoPDF
ckan_format_(geopdf).
ckan_format_(getdata).
% Geographic Infromation System (GIS)
ckan_format_(gis).
% Git
ckan_format_(git).
% Google Doc [Google]
ckan_format_('google doc').
% Google Sheet [Google]
ckan_format_('google sheet').
% GeoPackage (GPKG) [OGC]
ckan_format_(gpkg).
% General Transit Feed Specification (GTFS)
ckan_format_(gtfs).
% Shuttle Radar Topography Mission Height (SRTM) height (HGT) file
ckan_format_(hgt).
ckan_format_(hydra).
ckan_format_(hyperlink).
ckan_format_(image).
ckan_format_(index).
ckan_format_(iso).
ckan_format_(labpal).
ckan_format_(link).
ckan_format_('linked data').
ckan_format_(log).
ckan_format_(mabxml).
ckan_format_(map).
% MARC 21: redefinition of MARC for the 21st century
ckan_format_('marc21').
% MARCXML
ckan_format_(marcxml).
% MBTiles: file format for storing map tiles in a single file;
% technically, an SQLite database.
ckan_format_(mbtiles).
ckan_format_(none).
% ODATA
ckan_format_(odata).
% Open Geospatial Consortium [OGC]
ckan_format_(ogc).
% Open Streetmap (OSM)
ckan_format_(osm).
ckan_format_(other).
% Web Ontology Language (OWL) [W3C]
ckan_format_(owl).
% Protocolbuffer Binary Format (PBF)
ckan_format_(pbf).
% PHP
ckan_format_(php).
% ShapeFile-related [ESRI]
ckan_format_(prj).
% pixel image
ckan_format_(px).
% Python
ckan_format_(py).
% QGIS
ckan_format_(qgis).
% ArcGIS-related [ESRI]
ckan_format_(qlr).
ckan_format_(qual).
% R
ckan_format_(r).
% Resource Description Framework (RDF) [W3C]
ckan_format_(rdf).
% SPSS save file
ckan_format_(sav).
ckan_format_(scraper).
ckan_format_(sdf).
ckan_format_(search).
ckan_format_(service).
% ShapeFile [ESRI]
ckan_format_(shp).
% ShapeFile-related [ESRI]
ckan_format_(shx).
ckan_format_(sid).
ckan_format_('sisis export format').
% Solr search engine?
ckan_format_(solr).
% This could be either the query string format, or one of the result
% set formats.
ckan_format_(sparql).
% (document type)
ckan_format_(spreadsheet).
% SPSS Statistics
ckan_format_(spss).
% Structured Query Language (SQL)
ckan_format_(sql).
% SQLite
ckan_format_(sqlite).
% Search/Retrieve via URL (SRU): a standard search protocol for
% Internet search queries, utilizing Contextual Query Language (CQL),
% a standard query syntax for representing queries.
ckan_format_(sru).
ckan_format_(text).
% Translation Memory eXchange (TMX) is an XML specification for the
% exchange of translation memory data between computer-aided
% translation and localization tools with little or no loss of
% critical data.
ckan_format_(tmx).
ckan_format_(tomtom).
% TopoJSON: extension of GeoJSON
ckan_format_(topojson).
% [ESRI]
ckan_format_(twf).
% Universal Resource Locator (URL)
ckan_format_(url).
ckan_format_(void).
% Web Coverage Service (WCS)
ckan_format_(wcs).
ckan_format_(webapp).
% Web Application Resource (WAR)
ckan_format_(war).
ckan_format_(webgis).
% Web Feature Service (WFS)
ckan_format_(wfs).
% Web Map Service (WMS)
ckan_format_(wms).
% Web Map Tile Service (WMTS) [OGC]
ckan_format_(wmts).
% Web Services Description Language (WSDL)
ckan_format_(wsdl).
% World Wide Web (WWW)
ckan_format_(www).
% Excel toolbars file [Microsoft]
ckan_format_(xlb).
% XML Schema Definition (XSD) [W3C]
ckan_format_(xsd).

ckan_format_alias_(data, dat).
ckan_format_alias_(dbase, dbf).
ckan_format_alias_('esri shape', shapefile).
ckan_format_alias_('esri shape file', shapefile).
ckan_format_alias_('esri shape files', shapefile).
ckan_format_alias_(gdocs, 'google doc').
ckan_format_alias_(geopackage, gpkg).
ckan_format_alias_('google spreadsheet', 'google sheet').
ckan_format_alias_(googlespreadsheet, 'google sheet').
ckan_format_alias_(img, image).
ckan_format_alias_(multiformat, multipart).
ckan_format_alias_('ogc:wfs', wfs).
ckan_format_alias_('ogc:wms', wms).
% PASW (Predictive Analytics SoftWare)
ckan_format_alias_(pasw, spss).
ckan_format_alias_('plain text', text).
ckan_format_alias_(shape, shp).
ckan_format_alias_(shapefile, shp).
ckan_format_alias_(shapefiles, shp).
ckan_format_alias_('sql server', sql).
ckan_format_alias_(sqlitedb, sqlite).
ckan_format_alias_(txt, text).

ckan_media_type_(media(api/'linked-data',_)).
ckan_media_type_(media(api/dcat,_)).
ckan_media_type_(media(api/git,_)).
ckan_media_type_(media(api/search,_)).
ckan_media_type_(media(api/sparql,_)).
ckan_media_type_(media(application/'octet-stream',_)).
ckan_media_type_(media(application/'sql+gzip',_)).
ckan_media_type_(media(application/'vnd.lotus-1-2-3',_)).
ckan_media_type_(media(application/'vnd.lotus-approach',_)).
ckan_media_type_(media(application/'vnd.lotus-freelance',_)).
ckan_media_type_(media(application/'vnd.lotus-notes',_)).
ckan_media_type_(media(application/'vnd.lotus-organizer',_)).
ckan_media_type_(media(application/'vnd.lotus-screencam',_)).
ckan_media_type_(media(application/'vnd.lotus-wordpro',_)).
% Hierarchical Data Format (HDF)
ckan_media_type_(media(application/'x-hdf',_)).
% TROFF
ckan_media_type_(media(application/'x-troff-man',_)).
ckan_media_type_(media(application/'x-zip-compressed',_)).
ckan_media_type_(media(application/'zip+vnd.ms-excel',_)).
ckan_media_type_(media(application/download,_)).
ckan_media_type_(media(example/'*',_)).
ckan_media_type_(media(example/html,_)).
ckan_media_type_(media(example/rdf,_)).
ckan_media_type_(media(gdocs/spreadsheet,_)).
ckan_media_type_(media(index/ftp,_)).
ckan_media_type_(media(mapping/owl,_)).
ckan_media_type_(media(mapping/rdfs,_)).
ckan_media_type_(media(meta/'rdf+schema',_)).
ckan_media_type_(media(meta/'rdf-schema',_)).
ckan_media_type_(media(meta/owl,_)).
ckan_media_type_(media(meta/sitemap,_)).
ckan_media_type_(media(meta/void,_)).
ckan_media_type_(media(multipart/'form-data',_)).
ckan_media_type_(media(rdf/'xml, html, json',_)).
ckan_media_type_(media(rdf/void,_)).
ckan_media_type_(media(text/plain,_)).
ckan_media_type_(media(video/'x-msvideo',_)).

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
:- use_module(library(http/http_client2)).
:- use_module(library(ll/ll_seeder)).
:- use_module(library(media_type)).
:- use_module(library(thread_ext)).

:- dynamic
    ckan_site__/1.





%! ckan_add_seed(+Site:atom, +LMod:float, +Package:dict) is det.

ckan_add_seed(Site, LMod, Package) :-
  Url{name: DName, resources: Resources} :< Package,
  maplist(ckan_resource_url, Resources, Urls),
  % organization
  Org1 = _{url: Site},
  ckan_organization(Package, Org1, Org2),
  % description
  Dataset1 = _{name: DName, url: Url},
  ckan_description(Package, Dataset1, Dataset2),
  % license
  ckan_license(Package, Dataset2, Dataset3),
  add_seed(
    _{
      dataset: Dataset3,
      documents: Urls,
      'last-modified': LMod,
      organization: Org2
    }
  ).

ckan_description(Package, Dataset1, Dataset2) :-
  _{notes: Desc0} :< Package,
  Desc0 \== null, !,
  atom_string(Desc0, Desc),
  Dataset2 = Dataset1.put(_{description: Desc}).
ckan_description(_, Dataset, Dataset).

ckan_license(Package, Dataset1, Dataset2) :-
  _{license_url: License} :< Package, !,
  Dataset2 = Dataset1.put(_{license: License}).
ckan_license(_, Dataset, Dataset).

ckan_organization(Package, Org1, Org2) :-
  is_dict(Package.organization),
  % name
  _{name: OName} :< Package.organization, !,
  % image
  (   _{image_url: Url} :< Package.organization
  ->  Org2 = Org1.put(_{image: Url, name: OName})
  ;   Org2 = Org1.put(_{name: OName})
  ).
ckan_organization(_, Org, Org).

ckan_resource_url(Resource, Url) :-
  _{url: Url} :< Resource.



%! ckan_package_last_modified(+Package:dict, -LMod:float) is det.

ckan_package_last_modified(Package, LMod) :-
  _{resources: Resources} :< Package,
  aggregate_all(
    min(LMod),
    (
      member(Resource, Resources),
      ckan_resource_last_modified(Resource, LMod)
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
      findall(
        N-Format,
        retract_counter(ckan_unknown_format__(Site,Format), N),
        Pairs1
      ),
      forall(
        member(N-Format, Pairs1),
        format(Out, "~W\n", [format(Format,N),[quoted(true)]])
      ),
      findall(
        N-MediaType,
        retract_counter(ckan_unknown_media_type__(Site,MediaType), N),
        Pairs2
      ),
      forall(
        member(N-MediaType, Pairs2),
        format(Out, "~W\n", [format(MediaType,N),[quoted(true)]])
      )
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



%! ckan_resource_last_modified(+Resource:dict, -LMod:float) is det.

ckan_resource_last_modified(Resource, LMod) :-
  get_dict(last_modified, Resource, Str),
  Str \== null,
  parse_time(Str, iso_8601, LMod), !.
ckan_resource_last_modified(Resource, LMod) :-
  _{created: Str} :< Resource,
  parse_time(Str, iso_8601, LMod), !.
ckan_resource_last_modified(Resource, LMod) :-
  _{url: Url} :< Resource,
  (   http_metadata_last_modified(Url, LMod)
  ->  true
  ;   existence_error(last_modified, Url),
      LMod = 0.0
  ).



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



%! ckan_scrape_sites(+NumThreads:nonneg) is det.

ckan_scrape_sites(NumThreads) :-
  aggregate_all(set(Site), ckan_site_uri(Site), Sites),
  thread_create(
    threaded_maplist(NumThreads, ckan_scrape_site, Sites),
    _,
    [detached(true)]
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
ckan_format_('rss + xml',                   media(application/'rss+xml',[])).
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
