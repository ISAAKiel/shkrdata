# shkrdata

Package for downloading and preparing shkr data for archaeological analysis. The database contains archaeological information from the Early Iron Age in South-West Germany. This package is intended to simplify the data access as well as some basic steps of the preparation of the data. For this purpose `shkrdata` comprises function for accessing the data as well as function wrapping functionality from our package `moin` [https://github.com/CRC1266-A2/moin](https://github.com/CRC1266-A2/moin). While moin was developed in project A2 of the DFG [CRC 1266](http://www.sfb1266.uni-kiel.de?set_language=en) and in the summer school [*moin*](http://www.ufg.uni-kiel.de/de/aktuelles/events/tagungen-ausstellungen/copy_of_mosaic2018) in 2018, shkrdata was provided by DFG Project ['Modeling and reconstruction of interaction and distribution systems from the Early Iron Age in Southwest- and West Germany and Alsace'](http://gepris.dfg.de/gepris/projekt/252470382?language=en). 

## Database

The shkr database (Nakoinz 2013) is online available at [https://www.jma.uni-kiel.de/en/research-projects/data-exchange-platform](https://www.jma.uni-kiel.de/en/research-projects/data-exchange-platform).


## Installation

``` r
devtools::install_github("ISAAKiel/shkrdata")
```

## Functionality

**shkrdata::shkr2010_download**  downloads the 2010 version of the shkr data bank from [https://www.jma.uni-kiel.de/en/research-projects/data-exchange-platform.

**shkrdata::load_shkr2010**    loads the data into a list of tibbles.

**shkrdata::shkr_filter_loc10**   filters certain artefact types and groups of types.

**shkrdata::shkr_comp_loc10** transfers information from all levels to the artefact level (e. g. coordinates of the sites are attributed to the artefacts).

**shkrdata::shkr_coord_trans** the gaps in the site locations. In some cases the place is known but not the exact site location. Then the place coordinates are used to fill the empty site coordinates.

**shkrdata::shkr_filter_region**  filter a shkr tibble/dataframe to a certain spatial extent provided by coordinates or administrative units.

**shkrdata::shkr_filter_loc10_agg**    filters returns only artefacts from graves, settlements or fortifications.

**shkrdata::shkr_ts**   calculates type spectra of types (=TS) being a contingency table with graves, sites or something else as rows and types as columns. TS are calculated for one typological facet only, since the comparison of two facets is difficult and might be biased. The aggregation level, the units for which the occurrence of types is counted can be "grave", "site", "place" and "agg". In case of "grave" loc_08_id is used as key and in case of "site" loc_06_id is used. The user has to make sure, that the used features are restricted to graves. In case of "aggr", the user has to create a column named aggr which is used for this purpose.

**shkrdata::shkr_graph**  creates an igraph network object from a distance matrix.

**shkrdata::print_subtypes**  prints and returns the subtypes of a type represented by the type string containing a type code such as "B31". With the "descr" category it is also possible to submit a text string, for which is searched in the type description.


## References

Nakoinz, O. 2013. Archäologische Kulturgeographie der ältereisenzeitlichen Zentralorte Südwestdeutschlands. Bonn: Habelt.

## Licence

`shkrdata` is released under the [GNU General Public Licence, version 3](http://www.r-project.org/Licenses/GPL-3). Comments and feedback are welcome, as are code contributions.
