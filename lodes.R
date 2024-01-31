center_xy <- function(sdf) {
  sdf |>
    dplyr::mutate(
      point = sf::st_point_on_surface(geometry),
      x = sf::st_coordinates(point)[,1],
      y = sf::st_coordinates(point)[,2]
    ) |>
    dplyr::select(-point)
}

get_lodes <- function(states = CONFIG$states, 
                      year = CONFIG$year, 
                      census_unit = CONFIG$census_unit) {
  suppressMessages(lehdr::grab_lodes(
    state = states,
    year = year,
    version = "LODES8",
    job_type = "JT01",
    segment = "S000",
    state_part = "main",
    agg_geo = census_unit,
    use_cache = TRUE
  ))
}

prep_lodes <- function(od,
                       census_unit = CONFIG$census_unit) {
  h_col <- stringr::str_c("h", census_unit, sep = "_")
  w_col <- stringr::str_c("w", census_unit, sep = "_")
  od |>
    dplyr::rename(
      w_unit = {{w_col}},
      h_unit = {{h_col}}
    )
}

make_line <- function(xyxy){
  sf::st_linestring(
    base::matrix(
      xyxy, 
      nrow = 2, 
      byrow = TRUE
      )
    )
}

xyxy_to_lines <- function(df, crs, names = c("x_h","y_h","x_w","y_w")){
  #' Takes a dataframe with four columns containing two XY pairs (e.g., [X_{1}, 
  #' Y_{1}], [X_{2}, Y{2}]) and returns an sf dataframe with the same number of
  #' rows with those coordinates converted into linestrings.
  #'
  #' @param df A dataframe with four columns containing two XY pairs.
  #' @param crs Coordinate reference system.
  #' @param names list of the names of the four columns.
  #'
  #' @return A dataframe with LINESTRING geometries.
  #'
  #' @export
  
  sf::st_sf(
    df, 
    geometry = df |>
      dplyr::select(dplyr::all_of(names)) |>
      base::apply(1, make_line, simplify = FALSE) |>
      sf::st_sfc(crs = crs)
    ) |>
    dplyr::select(
      -all_of(names)
    )
}

select_place <- function(place_geo) {
  placenames_regex <- stringr::str_c("^", CONFIG$placename, collapse="|")
  place_matches <- stringr::str_detect(
    place_geo$pl_name, 
    placenames_regex
  )
  name_count <- sum(place_matches)
  match_string <- place_geo |> 
    dplyr::filter(place_matches) |> 
    dplyr::pull(pl_name) |>
    stringr::str_c(collapse=', ')
  if (all(stringr::str_c(CONFIG$placename) %in% place_geo$pl_name)) {
    message(glue::glue("Exact match found for place name(s): {stringr::str_c(CONFIG$placename, collapse=', ')}."))
    place_geo <- place_geo |>
      dplyr::filter(CONFIG$placename == pl_name)
  } else if (name_count == length(CONFIG$placename)) {
    message(glue::glue("Found closely matching place name(s): {match_string}."))
    place_geo <- place_geo |>
      dplyr::filter(place_matches)
  } else if (name_count > length(CONFIG$placename)) {
    message(glue::glue("Ambiguous place name---could refer to more than one place: {match_string}."))
  } else {
    message("Place not found.")
  }
  place_geo
}


census_units_to_places <- function(census_units, place_geo) {
  # This is necessary to suppress 'st_point_on_surface assumes attributes are 
  # constant over geometries' warning.
  sf::st_agr(census_units) <- "constant"
  census_units_pts <- census_units |>
    sf::st_point_on_surface() |>
    sf::st_join(place_geo) |>
    center_xy() 
  if ("placename" %in% names(CONFIG)) {
    census_units_pts <- census_units_pts |>
      sf::st_join(
        select_place(place_geo) |>
          dplyr::mutate(
            sel = TRUE
          ) |>
          dplyr::select(sel)
      ) |>
      tidyr::replace_na(
        list(sel = FALSE)
      )
  }
  sf::st_sf(
    census_units_pts |> sf::st_drop_geometry(),
    census_units |> sf::st_geometry()
    )
}

census_units_drop_cols <- function(census_units) {
  census_units |>
    dplyr::select(
      unit_id = GEOID,
      pl_id,
      pl_name,
      x,
      y,
      x_pl,
      y_pl,
      dplyr::any_of(
        "sel"
      )
    ) |>
    sf::st_drop_geometry()
}



lodes_to_census_units <- function(df, 
                                  census_points,
                                  census_unit = CONFIG$census_unit) {
  df |>
    dplyr::left_join(
      census_points |> 
        dplyr::rename(
          x_w = x,
          y_w = y,
          x_pl_w = x_pl,
          y_pl_w = y_pl,
          pl_w = pl_id,
          pl_n_w = pl_name,
          dplyr::any_of(c(sel_w = "sel"))
        ), 
      by = c("w_unit" = "unit_id")
    ) |>
    dplyr::left_join(
      census_points |> 
        dplyr::rename(
          x_h = x,
          y_h = y,
          x_pl_h = x_pl,
          y_pl_h = y_pl,
          pl_h = pl_id,
          pl_n_h = pl_name,
          dplyr::any_of(c(sel_h = "sel"))
        ),
      by = c("h_unit" = "unit_id")
    )
}

proximity_measures <- function(od_census_units) {
  prox <- od_census_units |>
    dplyr::mutate(
      in_unit = w_unit == h_unit,
      in_town = pl_h == pl_w
    ) |>
    tidyr::replace_na(
      list(
        in_town = FALSE
      )
    )
  
  pl_h_null <- prox |>
    dplyr::filter(!is.na(pl_h)) |>
    dplyr::pull(h_unit) |>
    base::unique()
  
  pl_w_null <- prox |>
    dplyr::filter(!is.na(pl_w)) |>
    dplyr::pull(w_unit) |>
    base::unique()
  
  # Commence hacky copypaste...
  # TODO: Fight with dplyr programming.
  # What % of jobs in tract are held by people in that tract?
  w_in_tract <- prox |>
    dplyr::group_by(in_unit, unit_id = w_unit) |>
    dplyr::summarize(
      count = sum(S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = unit_id,
      names_from = in_unit,
      names_prefix = "in_unit_",
      values_from = count,
      values_fill = 0
    ) |>
    dplyr::rename(
      w_in_unit = in_unit_TRUE
    ) |>
    dplyr::mutate(
      w_tot_in_unit = in_unit_FALSE + w_in_unit,
      pct_w_in_unit = w_in_unit / w_tot_in_unit * 100
    ) |>
    dplyr::select(-in_unit_FALSE)
  
  # What % of jobs in tract are held by people in town?
  w_in_town <- prox |>
    dplyr::group_by(in_town, unit_id = w_unit) |>
    dplyr::summarize(
      count = sum(S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = unit_id,
      names_from = in_town,
      names_prefix = "in_town_",
      values_from = count,
      values_fill = 0
    ) |>
    dplyr::rename(
      w_in_town = in_town_TRUE
    ) |>
    dplyr::mutate(
      w_tot_in_town = in_town_FALSE + w_in_town,
      pct_w_in_town = w_in_town / w_tot_in_town * 100
    ) |>
    dplyr::select(-in_town_FALSE) |>
    dplyr::filter(unit_id %in% pl_w_null)
  
  # What % of working residents work in tract?
  h_in_tract <- prox |>
    dplyr::group_by(in_unit, unit_id = h_unit) |>
    dplyr::summarize(
      count = sum(S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = unit_id,
      names_from = in_unit,
      names_prefix = "in_unit_",
      values_from = count,
      values_fill = 0
    ) |>
    dplyr::rename(
      h_in_unit = in_unit_TRUE
    ) |>
    dplyr::mutate(
      h_tot_in_unit = in_unit_FALSE + h_in_unit,
      pct_h_in_unit = h_in_unit / h_tot_in_unit * 100
    ) |>
    dplyr::select(-in_unit_FALSE)
  
  # What % of working residents work in town?
  h_in_town <- prox |>
    dplyr::group_by(in_town, unit_id = h_unit) |>
    dplyr::summarize(
      count = sum(S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = unit_id,
      names_from = in_town,
      names_prefix = "in_town_",
      values_from = count,
      values_fill = 0
    ) |>
    dplyr::rename(
      h_in_town = in_town_TRUE
    ) |>
    dplyr::mutate(
      h_tot_in_town = in_town_FALSE + h_in_town,
      pct_h_in_town = h_in_town / h_tot_in_town * 100
    ) |>
    dplyr::select(-in_town_FALSE) |>
    dplyr::filter(unit_id %in% pl_h_null)
  
  w_in_tract |>
    dplyr::left_join(w_in_town, by = "unit_id") |>
    dplyr::left_join(h_in_tract, by = "unit_id") |>
    dplyr::left_join(h_in_town, by = "unit_id")
}

selected_ods_poly <- function(od_census_units) {
  sel_workers <- od_census_units |>
    dplyr::filter(sel_w) |>
    dplyr::group_by(unit_id = h_unit, pl_n_w) |>
    dplyr::summarize(
      work_res = sum(S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = unit_id,
      names_from = pl_n_w,
      names_prefix = "work_res_",
      values_from = work_res
    )
  
  sel_residents <- od_census_units |>
    dplyr::filter(sel_h) |>
    dplyr::group_by(unit_id = w_unit, pl_n_h) |>
    dplyr::summarize(
      res_work = sum(S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = unit_id,
      names_from = pl_n_h,
      names_prefix = "res_work_",
      values_from = res_work
    )
  
  sel_workers |>
    dplyr::left_join(sel_residents, by = "unit_id")
}

ods_lines <- function(od_census_units, crs = CONFIG$crs) {
  if ("placename" %in% names(CONFIG)) {
    od_census_units <- od_census_units |>
      dplyr::filter(sel_h | sel_w)
  }
  od_census_units |>
  dplyr::rename(
    count = S000
  ) |>
  dplyr::select(
    w_unit,
    h_unit,
    count,
    x_h,
    y_h,
    x_w,
    y_w
  ) |>
  xyxy_to_lines(crs = crs)
}

ods_lines_place_agg <- function(od_census_units, crs = CONFIG$crs) {
  if ("placename" %in% names(CONFIG)) {
    od_census_units <- od_census_units |>
      dplyr::filter(sel_h | sel_w)
  }
  od_census_units |>
    dplyr::group_by(
      pl_n_h, 
      pl_n_w, 
      x_h = x_pl_h, 
      y_h = y_pl_h, 
      x_w = x_pl_w,
      y_w = y_pl_w
    ) |>
    dplyr::summarize(
      count = sum(S000)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      pl_n_h,
      pl_n_w,
      count,
      x_h,
      y_h,
      x_w,
      y_w
    ) |>
    tidyr::drop_na() |>
    xyxy_to_lines(crs = crs)
}