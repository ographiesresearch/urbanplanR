# UrbanPlanR

So far, the automated workflows download and process data from the **LEHD Origin-Destination Employment Statistics (LODES)** and the **5-year American Community Survey estimates**, including data necessary to produce ‘Petri Dish’ diagrams of place-based employment by industry and occupation.


## Setup

Currently, to set up the automated process, you modify [config.json](https://github.com/OGRAPHIES-Research-Design/urban-plannr/blob/main/config.json) with the following parameters:



1. **project – string** \
Name of the project. Currently, this only appears as the name of the output geopackage (or results folder, if format is ‘shapefile’ or ‘geojson’).
2. **states – string \
**Two-character abbreviation of state of interest. Currently, only one state is supported at a time, which might create analytical issues for places at the border.
3. **placename – string**
4. Name of place(s) (i.e., municipality) of interest. It/they must be in the state given in ‘states’. Can be a list.
5. **crs – integer**
6. Coordinate reference system EPSG code.
7. **census_unit – string**
8. Either ‘tracts’ or ‘block groups.’
9. **year – integer**
10. Year of interest. Need to identify ranges for each source. I’ve been using 2021.
11. **format – string**
12. One of “gpkg” (geopackage), “shp”, or “geojson.”
13. **census_api – string**
14. Optional. Census API key. It’s good practice to access the Census’s API with a credentialing key, though the scripts will run without one. [Request one here](https://api.census.gov/data/key_signup.html).


## Data Dictionary


### census_unit 🌎

Boundaries of a selected census unit in the state of interest: can be either block groups or tracts.


#### Geometry

MULTIPOLYGON


#### Fields



15. **unit_id – string** \
The unique identifier (AKA the FIPS code, often called the GEOID)
16. **name – string \
**Census tract ID.
17. **stusps - string**
18. Abbreviated state.
19. **namelsadco - string**
20. Name of county.
21. **pl_id – integer**
22. Unique identifier of place including the census geography.
23. **pl_name – string**
24. Name of the place including the census geography.
25. **sel – boolean**
26. Whether geography lies within the selected place.


### places_{state} 🌎

Boundaries of, in the case of Massachusetts, all municipalities and in other cases, census designated places in the selected state.


#### Geometry

MULTIPOLYGON


#### Fields



27. **pl_id – integer**
28. Unique identifier of place including the census geography.
29. **pl_name – string**
30. Name of the place including the census geography.


### places_selected 🌎

Boundaries of the particular places of interest (there can be more than one).


#### Geometry

MULTIPOLYGON


#### Fields



31. **pl_id – integer**
32. Unique identifier of place including the census geography.
33. **pl_name – string**
34. Name of the place including the census geography.


### census_unit_lodes

Table including measures derived from the LEHD Origin-Destination Employment Survey (LODES) data at the given census level. 


#### Geometry

None. 1-to-1 cardinality with census_unit by “unit_id” in both tables.


#### Fields



35. **unit_id – string** \
The unique identifier (AKA the FIPS code, often called the GEOID)
36. **work_res_{MUNI_NAME} – integer \
_(optional, only present if there is a selected placename)_** \
The number of workers who work in the selected municipality who commute from a home that lies within the given census geography.
37. **res_work_{MUNI_NAME} – integer** \
_(optional, only present if there is a selected placename) \
_The number of workers who live in the selected municipality who commute to a workplace that lies within given census geography.
38. **pct_w_in_town – float (%)** \
The % of workers who work in the census geography who also live in the town that the census area is in.
39. **pct_w_in_unit – float (%) \
**The % of workers who work in the census geography who also live in that census geography.
40. **pct_h_in_town – float (%) \
**The % of workers who live in the census geography who also live in the town that the census area is in.
41. **pct_h_in_unit – float (%)** \
The % of workers who live in the census geography who also work in that census geography.
42. 


### lodes_tract_lines 🌎

Non-aggregated tract-to-tract flows based on the LODES data.


#### Geometry

LINESTRING


#### Fields

**h_unit  – string**

Census geography of work. 1-to-many cardinality with **census_units **by **unit_hd = h_unit**

**w_unit  – string**

Census geography of home. 1-to-many cardinality with **census_units **by **unit_id = w_unit**

**count – integer**

The number of workers commuting from **h_unit** to **w_unit**.


### lodes_place_lines 🌎

Place-to-place (so, municipality-to-municipality) flows. This is much simpler to interpret because it's aggregated to the place.


#### Geometry

LINESTRING


#### Fields

**pl_n_h  – string**

Place name of home. 1-to-many cardinality with **places_{state} **by **pl_name = pl_n_h**

**pl_n_w  – string**

Place name of work. 1-to-many cardinality with **places_{state} **by **pl_name = pl_n_h**

**count – integer**

The number of workers commuting from pl_n_h to pl_n_w.


### occ_{area_type}_{depth}

These tables break down employment by occupation at various depths, or degrees of granularity based on ACS 5-year estimates of [Occupation by Sex for the Civilian Employed Population 16 Years and Over](https://data.census.gov/table/ACSST5Y2022.S2401). **_place_** indicates that we're looking at census designated places (generally, cities and towns). **_unit_** indicates they’re at the census unit level.

The _**{depth} **suffix indicates whether it’s looking at more generalized or more specific categories (i.e., where in the petri dish hierarchy it sits). Higher depth numbers indicate more specific categories, lower depth numbers are more general.


#### Geometry

None. occ_unit_{depth} has 1-to-1 cardinality with **census_unit** by **unit_id** in both tables.


#### Fields

Reference [this table](https://data.census.gov/table/ACSST5Y2022.S2401) for columns. They are stored as percentages of the total, so each row should sum to 100.


### ind_{area_type}_{depth}

These tables break down employment by industry at various depths, or degrees of granularity based on ACS 5-year estimates of [Industry by Sex for the Civilian Employed Population 16 Years and Older](https://data.census.gov/table/ACSST5Y2022.S2403). **_place_** indicates that we're looking at census designated places (generally, cities and towns). **_unit_** indicates they’re at the census unit level.

The _**{depth} **suffix indicates whether it’s looking at more generalized or more specific categories (i.e., where in the petri dish hierarchy it sits). Higher depth numbers indicate more specific categories, lower depth numbers are more general.


#### Geometry

None. ind_unit_{depth} has 1-to-1 cardinality with census_unit by **unit_id** in both tables.


#### Fields

Reference [this table](https://data.census.gov/table/ACSST5Y2022.S2403) for columns. They are stored as percentages of the total, so each row should sum to 100.
