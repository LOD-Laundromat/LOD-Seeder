:- module(
  ckan_analysis,
  [
    ckan_format/1 % -Format:pair
  ]
).

/** <module> CKAN analysis

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(pair_ext)).
:- use_module(library(readutil)).
:- use_module(library(settings)).

:- use_module(library(file_ext)).
:- use_module(library(ll/ll_seeder)).





%! ckan_format(-Format:pair(positive_integer,atom)) is nondet.

ckan_format(Pair) :-
  setting(ll_seeder:data_directory, Dir),
  findall(
    Label-N,
    (
      directory_path(Dir, File),
      call_stream_file(File, ckan_format_stream(Label, N))
    ),
    Pairs1
  ),
  group_pairs_by_key(Pairs1, Pairs2),
  maplist(sum_value, Pairs2, Pairs3),
  transpose_pairs(Pairs3, Pairs4),
  sort(1, @>=, Pairs4, Pairs5),
  member(Pair, Pairs5).

ckan_format_stream(Label, N, In) :-
  repeat,
  read_line_to_string(In, Line),
  (   Line == end_of_file
  ->  !, fail
  ;   read_term_from_atom(Line, Term, [])
  ),
  (Term = format(Label,N) ; Term = media_type(Label,N)).

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
