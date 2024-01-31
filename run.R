source('globals.R')
source('lodes.R')
source('ind_occ.R')

run <- function() {
  message("Downloading and processing LEHD Origin-Destination Employment 
          Statistics (LODES) data...")
  od <- get_lodes() |>
    prep_lodes()
  
  message("Downloading places...")
  place_geo <- place_decision() |>
    write_multi(glue::glue("places_{tolower(CONFIG$states)}"))
  
  if ("placename" %in% names(CONFIG)) {
    place_geo |>
      select_place() |>
      write_multi("places_selected")
  }
  
  census_unit_locs <- get_census_units() |> 
    census_units_to_places(place_geo)
  
  census_unit_locs |>
    dplyr::select(-dplyr::starts_with(c("x", "y"))) |>
    write_multi(glue::glue("census_{CONFIG$census_unit}")) 
  
  od_census_units <- od |>
    lodes_to_census_units(
      census_unit_locs |>
        census_units_drop_cols()
      )
  
  census_units_measured <- od_census_units |>
    proximity_measures()
    
  
  if ("placename" %in% names(CONFIG)) {
    census_units_measured <- od_census_units |>
      selected_ods_poly() |>
      dplyr::left_join(census_units_measured, by="unit_id")
  }
  
  census_units_measured |>
    write_multi(glue::glue("census_{CONFIG$census_unit}_lodes"))
  
  ods_lines(od_census_units) |>
    write_multi(glue::glue("lodes_{CONFIG$census_unit}_lines"))
  
  ods_lines_place_agg(od_census_units) |>
    write_multi("lodes_place_lines")
  
  message("Downloading ACS occupation estimates...")
  get_occupations()
  message("Downloading ACS industry estimates...")
  get_industries()
}

if(!interactive()){
  renv::init()
  run()
}