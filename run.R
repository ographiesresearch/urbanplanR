source('R/globals.R')
source('R/lodes.R')
source('R/acs.R')

run <- function() {
  
  message("Downloading places...")
  place_geo <- place_decision()
  
  if ("placenames" %in% names(CONFIG)) {
    place_geo <- place_geo |>
      select_places()
  }
  
  place_geo |>
    remove_coords() |>
    write_multi("places")
  
  census_units <- get_census_units() |> 
    st_join_max_overlap(place_geo, x_id = "unit_id", y_id = "pl_id")
  
  census_units |>
    remove_coords() |>
    write_multi(
      "census_unit"
      )
  
  if ("lodes" %in% CONFIG$datasets) {
    message("Downloading and processing LEHD Origin-Destination Employment Statistics (LODES) data...")
    od <- get_lodes() |>
      prep_lodes()
    
    od_census_units <- od |>
      lodes_to_census_units(census_units)
    
    census_units_measured <- od_census_units |>
      proximity_measures() |>
      dplyr::filter(unit_id %in% census_units$unit_id)
    
    if ("placenames" %in% names(CONFIG)) {
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
      write_multi(glue::glue("census_unit_lodes"))
    
    ods_lines(od_census_units) |>
      write_multi(glue::glue("lodes_unit_lines"))
    
    ods_lines_place_agg(od_census_units) |>
      write_multi("lodes_place_lines")
  }
  if ("occ" %in% CONFIG$datasets) {
    message("Downloading ACS occupation estimates...")
    get_occupations()
  }
  
  if ("ind" %in% CONFIG$datasets) {
    message("Downloading ACS industry estimates...")
    get_industries()
  }
}

if(!interactive()){
  renv::init()
  run()
}