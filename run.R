source('R/globals.R')
source('R/lodes.R')
source('R/acs.R')

options(
  # Suppress `summarise()` has grouped output by 'x'...'z' message.
  dplyr.summarise.inform = FALSE,
  # Suppress read/write CSV progress bar.
  readr.show_progress = FALSE
)

run <- function(config) {
  if (class(config) == "character") {
    config <- jsonlite::read_json(config)
  }

  config <- config |>
    set_census_api() |>
    lehd_census_units() |>
    tidy_census_units() |>
    std_format()

  message("Downloading places...")
  place_geo <- place_decision(config$states, crs = config$crs)

  if ("places" %in% names(config)) {
    place_geo <- place_geo |>
      select_places(places = config$places)
  }

  get_places(states = config$states, year = config$year, crs = config$crs) |>
    write_multi("census_places", config = config)
  
  place_geo |>
    remove_coords() |>
    write_multi("places", config = config)
  
  census_units <- get_census_units(
    states = config$states,
    year = config$year,
    crs = config$crs,
    census_unit = config$census_unit
    ) |>
    st_join_max_overlap(place_geo, x_id = "unit_id", y_id = "pl_id")
  
  census_units |>
    remove_coords() |>
    write_multi("census_unit", config = config)
  
  if ("lodes" %in% config$datasets) {
    message("Downloading and processing LEHD Origin-Destination Employment Statistics (LODES) data...")
    od <- get_lodes(
        states = config$states,
        year = config$year,
        census_unit = config$census_unit) |>
      prep_lodes(
        census_unit = config$census_unit
      )
  
    od_census_units <- od |>
      lodes_to_census_units(
        census_units_geo = census_units,
        census_unit = config$census_unit
        )
  
    census_units_measured <- od_census_units |>
      proximity_measures() |>
      dplyr::filter(unit_id %in% census_units$unit_id)
  
    if ("places" %in% names(config)) {
      census_units_measured <- census_units_measured |>
          dplyr::full_join(
            od_census_units |>
              selected_ods_poly(),
            by = "unit_id"
            ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::where(is.numeric), ~tidyr::replace_na(.x, 0)
          )
        )
    }
  
    census_units_measured |>
      write_multi("census_unit_lodes", config = config)
  
    ods_lines(od_census_units, crs = config$crs) |>
      write_multi("census_unit_lodes_lines", config = config)
  
    ods_lines_place_agg(od_census_units, crs = config$crs) |>
      write_multi("place_lodes_lines", config = config)
  }
  
  
  if ("age" %in% config$datasets) {
    get_acs_age(states = config$states,
                year = config$year,
                census_unit = config$census_unit) |>
      write_multi("census_unit_acs_age", config = config)
  
    get_acs_age(states = config$states,
                year = config$year,
                census_unit = "place") |>
      write_multi("place_acs_age", config = config)
  }
  
  if ("race" %in% config$datasets) {
    get_acs_race(states = config$states,
                 year = config$year,
                 census_unit = config$census_unit) |>
      write_multi("census_unit_acs_race", config = config)
  
    get_acs_race(states = config$states,
                 year = config$year,
                 census_unit = "place")  |>
      write_multi("place_acs_race", config = config)
  }
  
  if ("housing" %in% config$datasets) {
    get_acs_housing(states = config$states,
                    year = config$year,
                    census_unit = config$census_unit) |>
      write_multi("census_unit_acs_housing", config = config)
  
    get_acs_housing(states = config$states,
                    year = config$year,
                    census_unit = "place")|>
      write_multi("place_acs_housing", config = config)
  }
  
  if ("occ" %in% config$datasets) {
    message("Downloading ACS occupation estimates...")
    get_acs_occupations(states = config$states,
                    year = config$year,
                    census_unit = config$census_unit) |>
      pivot_and_write(name = "census_unit_acs_occ", config = config)
  
    get_acs_occupations(states = config$states,
                    year = config$year,
                    census_unit = "place") |>
      pivot_and_write(name = "place_acs_occ", config = config)
  }
  
  if ("ind" %in% config$datasets) {
    message("Downloading ACS industry estimates...")
    get_acs_industries(states = config$states,
                   year = config$year,
                   census_unit = config$census_unit)  |>
      pivot_and_write(name = "census_unit_acs_ind", config = config)
  
    get_acs_industries(states = config$states,
                    year = config$year,
                    census_unit = "place") |>
      pivot_and_write(name = "place_acs_ind", config = config)
  }
}

if(!interactive()){
  renv::init()
  run(get_config(commandArgs(trailingOnly = TRUE)))
}