source('R/globals.R')
source('R/lodes.R')
source('R/acs.R')

options(
  # Suppress `summarise()` has grouped output by 'x'...'z' message.
  dplyr.summarise.inform = FALSE,
  # Suppress read/write CSV progress bar.
  readr.show_progress = FALSE
)

CONFIG <- jsonlite::read_json('config.json')

run <- function(config = CONFIG) {
  if (class(config) == "character") {
    config <- jsonlite::read_json(config)
  }
  
  config <- config |>
    set_census_api() |>
    lehd_census_units() |>
    tidy_census_units() |>
    std_format()
  
  write_with_settings <- function(df, name) {
    write_multi(
      df,
      name,
      dir_name = config$project, 
      format = config$format
    )
  }
  
  message("Downloading places...")
  place_geo <- place_decision(config$states)
  
  if ("places" %in% names(config)) {
    place_geo <- place_geo |>
      select_places(places = config$places)
  }

  place_geo |>
    remove_coords() |>
    write_with_settings("places")

  census_units <- get_census_units(
    states = config$states,
    year = config$year,
    crs = config$crs,
    census_unit = config$census_unit
    ) |>
    st_join_max_overlap(place_geo, x_id = "unit_id", y_id = "pl_id")

  census_units |>
    remove_coords() |>
    write_with_settings(
      "census_unit"
      )

  if ("lodes" %in% config$datasets) {
    message("Downloading and processing LEHD Origin-Destination Employment Statistics (LODES) data...")
    od <- get_lodes(
        states = config$states,
        year = congig$year,
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
      write_with_settings(glue::glue("census_unit_lodes"))
    
    ods_lines(od_census_units, crs = config$crs) |>
      write_with_settings(glue::glue("lodes_unit_lines"))
    
    ods_lines_place_agg(od_census_units, crs = config$crs) |>
      write_with_settings("lodes_place_lines")
  }
  # 
  # 
  # if ("age" %in% CONFIG$datasets) {
  #   get_acs_age() |>
  #     write_with_settings("acs_age")
  #   
  #   get_acs_age(census_unit = "place") |>
  #     write_with_settings("acs_age_place")
  # }
  # 
  # if ("race" %in% CONFIG$datasets) {
  #   get_acs_race() |>
  #     write_with_settings("acs_race")
  #   
  #   get_acs_race(census_unit = "place") |>
  #     write_with_settings("acs_race_place")
  # }
  # 
  # if ("housing" %in% CONFIG$datasets) {
  #   get_acs_housing() |>
  #     write_with_settings("acs_housing")
  #   
  #   get_acs_housing(census_unit = "place") |>
  #     write_with_settings("acs_housing_place")
  # }
  # 
  # if ("occ" %in% CONFIG$datasets) {
  #   message("Downloading ACS occupation estimates...")
  #   get_occupations()
  # }
  # 
  # if ("ind" %in% CONFIG$datasets) {
  #   message("Downloading ACS industry estimates...")
  #   get_industries()
  # }
}

if(!interactive()){
  renv::init()
  run()
}