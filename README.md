# UrbanPlanR

So far, the automated workflows download and process data from the **LEHD Origin-Destination Employment Statistics (LODES)** and the **5-year American Community Survey estimates**, including data necessary to produce ‘Petri Dish’ diagrams of place-based employment by industry and occupation. Developed in collaboration with [Utile Architecture and Planning](https://www.utiledesign.com/).


## Setup

Currently, to set up the automated process, you modify [config.json](https://github.com/OGRAPHIES-Research-Design/urban-plannr/blob/main/config.json) with the following parameters:

+ **project – string** \
Name of the project. Currently, this only appears as the name of the output geopackage (or results folder, if format is ‘shapefile’ or ‘geojson’).
+ **states – array of strings** \
Array of states of interest using standard two-letter abbreviations. This can be understood as the 'region' of study in the broadest sense, allowing for analysis of, for example, commuter flows over state lines.
+ **placenames – array of objects** \
_Optional._ An object containing towns/cities of interest. It/they must be in the states provided as ‘states’. Each object should have the following two properties...
    + **place – string** \
    Name of each town/city of interest.
    + **state – string** \
    Two-letter abreviation of state including indicated place.
+ **crs – integer** \
Coordinate reference system EPSG code.
+ **census_unit – string** \
Either ‘tracts’ or ‘block groups.’
+ **year – integer** \
Year of interest. Need to identify ranges for each source. I’ve been using 2021.
+ **format – string** \
One of “gpkg” (geopackage), “shp” (shapefile), or “geojson.”
+ **datasets – array of strings** \
List of datasets to download and write. Current possible values are...
    + `"lodes"`: Tables derviced from the LEHD origin-destination employment statistics database.
    + `"occ"`: Occupation of civilian employed population 16 and over.
    + `"ind"`: Industry of civilian employed population 16 and over.
+ **census_api – string** \
_Optional._ Census API key. It’s good practice to access the Census’s API with a credentialing key, though the scripts will run without one. [Request one here](https://api.census.gov/data/key_signup.html).

For example, a `config.json` for a project focused on Salem, MA that also includes adjacent Beverly, MA  as a place of interest (and includes census data for all six states that make up the New England region) would look like this...

```json
{
  "project": "salem",
  "states": ["MA", "ME", "NH", "VT", "CT", "RI"],
  "placenames": [
    {
      "place": "Salem",
      "state": "MA"
    },
    {
      "place": "Beverly",
      "state": "MA"
    }
  ],
  "crs": 2249,
  "census_unit": "tracts",
  "year": 2021,
  "format": "gpkg",
  "datasets": ["lodes", "occ", "ind"],
  "census_api": "your_api_key"
}
```

## Data Dictionary


### census_unit 🌎

Boundaries of a selected census unit in the state of interest: can be either block groups or tracts.


#### Geometry

MULTIPOLYGON


#### Fields



+ **unit_id – string** \
The unique identifier (AKA the FIPS code, often called the GEOID)
+ **name_long - string** \
Place state pair used to uniquely identify tract's place.
+ **pl_name – string** \
Name of the place including the census geography.
+ **selected – boolean** \
Whether geography lies within the selected place(s).


### places 🌎

Boundaries of, in the case of Massachusetts, all municipalities and in other cases, census designated places in the selected state.


#### Geometry

MULTIPOLYGON


#### Fields



+ **name_long - string** \
Place state pair used to uniquely identify  place.
+ **pl_name – string** \
Name of the place.
+ **state – string** \
Two-letter abbreviation of state in which geography falls.
+ **selected – boolean** \
Whether place is selected.


### census_unit_lodes

Table including measures derived from the LEHD Origin-Destination Employment Survey (LODES) data at the given census level. 


#### Geometry

None. 1-to-1 cardinality with census_unit by “unit_id” in both tables.


#### Fields



+ **unit_id – string** \
The unique identifier (AKA the FIPS code, often called the GEOID)
+ **work_res_{MUNI_NAME} – integer** \
_(optional, only present if there is a selected placename)_ \
The number of workers who work in the selected municipality who commute from a home that lies within the given census geography.
+ **res_work_{MUNI_NAME} – integer** \
_(optional, only present if there is a selected placename)_ \
The number of workers who live in the selected municipality who commute to a workplace that lies within given census geography.
+ **pct_w_in_town – float (%)** \
The % of workers who work in the census geography who also live in the town that the census area is in.
+ **pct_w_in_unit – float (%)** \
The % of workers who work in the census geography who also live in that census geography.
+ **pct_h_in_town – float (%)** \
The % of workers who live in the census geography who also live in the town that the census area is in.
+ **pct_h_in_unit – float (%)** \
The % of workers who live in the census geography who also work in that census geography.

### lodes_unit_lines 🌎

Non-aggregated unit-to-unit flows based on the LODES data.


#### Geometry

LINESTRING


#### Fields

+ **h_unit  – string** \
Census geography of work. 1-to-many cardinality with **census_units **by **unit_hd = h_unit**
+ **h_selected  – boolean** \
Used to select only commutes from a home in the selected place.
+ **w_unit  – string** \
Census geography of home. 1-to-many cardinality with **census_units **by **unit_id = w_unit**
+ **w_selected  – boolean** \
Used to select only commutes to workplaces in the selected place.
+ **count – integer** \
The number of workers commuting from **h_unit** to **w_unit**.


### lodes_place_lines 🌎

Place-to-place (so, municipality-to-municipality) flows. This is much simpler to interpret because it's aggregated to the place.


#### Geometry

LINESTRING


#### Fields

+ **pl_n_h  – string** \
Place name of home. 1-to-many cardinality with **places_{state} **by **pl_name = pl_n_h**
+ **h_selected  – boolean** \
Used to select only commutes from a home in the selected place.
+ **pl_n_w  – string** \
Place name of work. 1-to-many cardinality with **places_{state} **by **pl_name = pl_n_h**
+ **w_selected  – boolean** \
Used to select only commutes to workplaces in the selected place.
+ **count – integer** \
The number of workers commuting from pl_n_h to pl_n_w.


### occ_{area_type}_{depth}

These tables break down employment by occupation at various depths, or degrees of granularity based on ACS 5-year estimates of [Occupation by Sex for the Civilian Employed Population 16 Years and Over](https://data.census.gov/table/ACSST5Y2022.S2401). **place** indicates that we're looking at census designated places (generally, cities and towns). **unit** indicates they’re at the census unit level.

The **{depth}** suffix indicates whether it’s looking at more generalized or more specific categories (i.e., where in the petri dish hierarchy it sits). Higher depth numbers indicate more specific categories, lower depth numbers are more general.


#### Geometry

None. occ_unit_{depth} has 1-to-1 cardinality with **census_unit** by **unit_id** in both tables.


#### Fields

Reference [this table](https://data.census.gov/table/ACSST5Y2022.S2401) for columns. They are stored as percentages of the total, so each row should sum to 100.


### ind_{area_type}_{depth}

These tables break down employment by industry at various depths, or degrees of granularity based on ACS 5-year estimates of [Industry by Sex for the Civilian Employed Population 16 Years and Older](https://data.census.gov/table/ACSST5Y2022.S2403). **place** indicates that we're looking at census designated places (generally, cities and towns). **unit** indicates they’re at the census unit level.

The **{depth}** suffix indicates whether it’s looking at more generalized or more specific categories (i.e., where in the petri dish hierarchy it sits). Higher depth numbers indicate more specific categories, lower depth numbers are more general.


#### Geometry

None. ind_unit_{depth} has 1-to-1 cardinality with census_unit by **unit_id** in both tables.


#### Fields

Reference [this table](https://data.census.gov/table/ACSST5Y2022.S2403) for columns. They are stored as percentages of the total, so each row should sum to 100.
