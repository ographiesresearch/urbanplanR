get_lodes <- function(states = CONFIG$states, 
                      year = CONFIG$year, 
                      census_unit = CONFIG$census_unit) {
  lodes_list <- list()
  for (st in states) {
    message(glue::glue("Getting LODES data for {st}."))
    lodes_list[[st]] <- lehdr::grab_lodes(
      state = st,
      year = year,
      version = "LODES8",
      job_type = "JT01",
      segment = "S000",
      state_part = "main",
      agg_geo = census_unit,
      use_cache = TRUE
    ) |>
      dplyr::bind_rows(
        lehdr::grab_lodes(
          state = st,
          year = year,
          version = "LODES8",
          job_type = "JT01",
          segment = "S000",
          state_part = "aux",
          agg_geo = census_unit,
          use_cache = TRUE
        )
      )
  }
  dplyr::bind_rows(lodes_list) |>
    dplyr::distinct()
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

select_places <- function(place_geo) {
  place_searches <- dplyr::bind_rows(CONFIG$placenames) |>
    dplyr::mutate(
      place_long = stringr::str_c(place, state, sep=", "),
      regex = stringr::str_c("^", place_long, "$", sep="")
    ) |>
    dplyr::pull(regex) |>
    stringr::str_c(collapse="|")
  
  places <- dplyr::bind_rows(CONFIG$placenames) |>
    dplyr::pull(place)
  
  matched <- place_geo |>
    dplyr::mutate(
      selected = stringr::str_detect(name_long, place_searches)
    )
  match_count <- nrow(matched |> dplyr::filter(selected))
  if (match_count == length(CONFIG$placenames)) {
    message(glue::glue("Exact match found for all place names ({stringr::str_c(places, collapse=', ')})."))
  } else if (match_count > length(CONFIG$placenames)) {
    message(glue::glue("Ambiguous place names provided."))
    stop()
  } else {
    message(glue::glue("Unable to match all place names."))
    stop()
  }
  matched
}


census_units_to_places <- function(census_units, place_geo) {
  # This is necessary to suppress 'st_point_on_surface assumes attributes are 
  # constant over geometries' warning.
  sf::st_agr(census_units) <- "constant"
  sf::st_agr(place_geo) <- "constant"
  max_int <- census_units |>
    sf::st_intersection(
      dplyr::select(place_geo, name_long)
    ) |>
    dplyr::mutate(
      area = sf::st_area(geometry)
    ) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(unit_id) |>
    dplyr::slice_max(order_by = area) |>
    dplyr::ungroup() |>
    dplyr::select(-area)
  
  census_units |>
    dplyr::left_join(max_int, by="unit_id") |>
    dplyr::left_join(sf::st_drop_geometry(place_geo), by="name_long")
}

lodes_to_census_units <- function(df, 
                                  census_units,
                                  census_unit = CONFIG$census_unit) {
  
  census_units <- census_units |>
    center_xy() |>
    sf::st_drop_geometry() |>
    dplyr::select(-c(state, pl_name))
  
  df |>
    dplyr::left_join(
      census_units |> 
        dplyr::rename(
          x_w = x,
          y_w = y,
          x_pl_w = x_pl,
          y_pl_w = y_pl,
          pl_n_w = name_long,
          dplyr::any_of(c(selected_w = "selected"))
        ), 
      by = c("w_unit" = "unit_id")
    ) |>
    dplyr::left_join(
      census_units |> 
        dplyr::rename(
          x_h = x,
          y_h = y,
          x_pl_h = x_pl,
          y_pl_h = y_pl,
          pl_n_h = name_long,
          dplyr::any_of(c(selected_h = "selected"))
        ),
      by = c("h_unit" = "unit_id")
    )
}

prox_workers_in_unit <- function(prox) {
  prox |>
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
}

prox_workers_in_town <- function(prox) {
  prox |>
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
    dplyr::select(-in_town_FALSE)
}

prox_residents_in_unit <- function(prox) {
  # What % of working residents work in tract?
  prox |>
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
}

prox_residents_in_town <- function(prox) {
  prox |>
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
    dplyr::select(-in_town_FALSE)
}

proximity_measures <- function(od_census_units) {
  prox <- od_census_units |>
    dplyr::mutate(
      in_unit = w_unit == h_unit,
      in_town = pl_n_h == pl_n_w
    ) |>
    tidyr::replace_na(
      list(
        in_town = FALSE
      )
    )
  
  # Commence hacky copypaste...
  # TODO: Fight with dplyr programming.
  # What % of jobs in tract are held by people in that tract?
  prox_workers_in_unit(prox) |>
    dplyr::full_join(prox_workers_in_town(prox), by = "unit_id") |>
    dplyr::full_join(prox_residents_in_unit(prox), by = "unit_id") |>
    dplyr::full_join(prox_residents_in_town(prox), by = "unit_id")
}

selected_ods_poly <- function(od_census_units) {
  sel_workers <- od_census_units |>
    dplyr::filter(selected_w) |>
    dplyr::group_by(unit_id = h_unit, pl_n_w) |>
    dplyr::summarize(
      work_res = sum(S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = unit_id,
      names_from = pl_n_w,
      names_prefix = "work_res_",
      values_from = work_res,
      values_fill = 0
    )
  
  sel_residents <- od_census_units |>
    dplyr::filter(selected_h) |>
    dplyr::group_by(unit_id = w_unit, pl_n_h) |>
    dplyr::summarize(
      res_work = sum(S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = unit_id,
      names_from = pl_n_h,
      names_prefix = "res_work_",
      values_from = res_work,
      values_fill = 0
    )
  
  sel_workers |>
    dplyr::full_join(sel_residents, by = "unit_id")
}

ods_lines <- function(od_census_units, crs = CONFIG$crs) {
  if ("placenames" %in% names(CONFIG)) {
    od_census_units <- od_census_units |>
      dplyr::filter(selected_h | selected_w)
  }
  od_census_units |>
    tidyr::drop_na(x_h, y_h, x_w, y_w) |>
    dplyr::rename(
      count = S000
    ) |>
    dplyr::select(
      w_unit,
      selected_w,
      h_unit,
      selected_h,
      count,
      x_h,
      y_h,
      x_w,
      y_w
    ) |>
    xyxy_to_lines(crs = crs)
}

ods_lines_place_agg <- function(od_census_units, crs = CONFIG$crs) {
  if ("placenames" %in% names(CONFIG)) {
    od_census_units <- od_census_units |>
      dplyr::filter(selected_h | selected_w)
  }
  od_census_units |>
    tidyr::drop_na(x_h, y_h, x_w, y_w) |>
    dplyr::group_by(
      pl_n_h, 
      selected_h,
      pl_n_w, 
      selected_w,
      x_h = x_pl_h, 
      y_h = y_pl_h, 
      x_w = x_pl_w,
      y_w = y_pl_w
    ) |>
    dplyr::summarize(
      count = sum(S000)
    ) |>
    dplyr::ungroup() |>
    xyxy_to_lines(crs = crs)
}