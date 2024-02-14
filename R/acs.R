pct_transform <- function(df, unique_col) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(unique_col))) |>
    dplyr::mutate(
      estimate = dplyr::case_when(
        estimate == max(estimate) ~ 100,
        .default = estimate / max(estimate) * 100
      )
    ) |>
    dplyr::ungroup()
}

get_acs_vars <- function(vars,
                         states = CONFIG$states,
                         year = CONFIG$year,
                         county = NULL,
                         census_unit = CONFIG$census_unit,
                         geometry = TRUE,
                         crs = CONFIG$crs,
                         drop_moe = TRUE) {
  var_values <- unname(vars)
  df <- tidycensus::get_acs(
    geography = census_unit,
    variables = var_values, 
    year = year,
    state = states,
    county = county,
    geometry = TRUE,
    cache_table = TRUE,
    output = "wide"
  ) |>
    dplyr::rename_with(~stringr::str_remove(.x, "E$"))
  if (geometry) {
    df <- df |>
      sf::st_transform(crs)
  }
  if (drop_moe) {
    df <- df |>
      dplyr::select(
        -tidyselect::ends_with("M")
      )
  }
  if(!is.null(names(vars))) {
    df <- df |>
      dplyr::rename(
        dplyr::all_of(vars)
      )
  }
}

get_acs_table <- function(table, 
                          states = CONFIG$states, 
                          year = CONFIG$year,
                          census_unit = CONFIG$census_unit,
                          var_match = "",
                          var_suffix = TRUE) {
  tidy_census_units()
  df <- tidycensus::get_acs(
      geography = census_unit,
      table = table, 
      year = year,
      state = states,
      geometry = FALSE,
      cache_table = TRUE
    ) |>
    dplyr::filter(
      !stringr::str_detect(variable, "_C0[2-9]_")
    ) |>
    dplyr::mutate(
      prefix = stringr::str_c(
        "B",
        stringr::str_sub(table, 2),
        sep = ""
      ),
      prefix = dplyr::case_when(
        var_suffix ~ stringr::str_c(
          prefix,
          "1",
          sep = ""
        ),
        .default = prefix
      ),
      variable = stringr::str_c(
        prefix,
        stringr::str_extract(variable, "(?<=_)\\d+$"),
        sep="_"
      )
    ) |>
    dplyr::select(-prefix) |>
    dplyr::left_join(
      tidycensus::load_variables(
        year = year,
        dataset = "acs5",
        cache = TRUE
      ) |>
        dplyr::select(name, label),
      by = c("variable" = "name")
    ) |>
    dplyr::mutate(
      level = stringr::str_count(label, "!!") - 1,
      label = stringr::str_extract(
        label,
        "(?<=!!)[0-9A-Za-z\\s,()-áéíóúüñç]+(?=(?:$|:$))"
      )
    )

  if (nchar(var_match) > 0) {
    df <- df |>
      dplyr::filter(
        stringr::str_detect(variable, pattern = var_match)
      )
  }
  df
}

process_nested_table <- function(df) {
  max_level <- max(df$level)
  
  df <- df |>
    dplyr::mutate(
      levels_flag = dplyr::case_when(
        (level == dplyr::lead(level)) ~ TRUE,
        (level == dplyr::lag(level)) & (level > dplyr::lead(level)) ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      levels = dplyr::case_when(
        levels_flag ~ list(seq.int(level, max_level, 1)),
        .default = list(level)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label = stringr::str_replace_all(
        label,
        c("[(),:]" = "", "\\-" = " ", "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ü" = "u", "ñ" = "n", "ç" = "c")
      )
    ) |>
    dplyr::rename(
      unit_id = GEOID
    )
}

process_places <- function(df) {
  df |>
    dplyr::mutate(
      city = stringr::str_detect(
        NAME, "city,"
      )
    )
}

pivot_and_write <- function(df, name, percent = TRUE, unique_cols = c("unit_id")) {
  depths <- unique(df$level)
  depths_nonzero <- depths[ !depths == 0 ]
  
  if (percent) {
    df <- df |>
      pct_transform("unit_id") |>
      dplyr::mutate(
        type = "pct"
      ) |>
      dplyr::filter(label != "Total") |>
      dplyr::bind_rows(
        df |>
          dplyr::mutate(
            type = "count"
          )
      )
  } else {
    df <- df |>
      dplyr::mutate(
        type = "count"
      )
  }
  
  for (d in depths_nonzero) {
    df_out <- df |>
      dplyr::rowwise() |>
      dplyr::filter(
        d %in% levels || 0 %in% levels
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        label = stringr::str_replace_all(
          label,
          c("[(),:]" = "", "\\-" = " ", "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ü" = "u", "ñ" = "n", "ç" = "c")
        )
      ) |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of("unit_id"),
        names_from = c(type, label),
        names_glue = "{type}_{label}",
        values_from = estimate
      ) |>
      write_multi(glue::glue("{name}_depth_{d}"))
  }
  df_out
}

get_occupations <- function(census_unit = CONFIG$census_unit) {
  # Industry data from ACS Table S2401: Occupation by Sex for the Civilian 
  # Employed Population 16 Years and Over
  # https://data.census.gov/table/ACSST5Y2022.S2401
  suppressMessages(get_acs_table("S2401", census_unit = census_unit)) |>
    process_nested_table() |>
    pivot_and_write(name = "occ_unit")
  
  suppressMessages(get_acs_table("S2401", census_unit = "place")) |>
    process_nested_table() |>
    process_places() |>
    pivot_and_write(name = "occ_place", unique_col = c("unit_id", "name"))
}

get_occupations <- function(census_unit = CONFIG$census_unit) {
  # Industry data from ACS Table S2401: Occupation by Sex for the Civilian 
  # Employed Population 16 Years and Over
  # https://data.census.gov/table/ACSST5Y2022.S2401
  suppressMessages(get_acs_table("S2401", census_unit = census_unit)) |>
    process_nested_table() |>
    pivot_and_write(name = "occ_unit")
  
  suppressMessages(get_acs_table("S2401", census_unit = "place")) |>
    process_nested_table() |>
    process_places() |>
    pivot_and_write(name = "occ_place", unique_col = c("unit_id", "name"))
}

get_industries <- function(census_unit = CONFIG$census_unit) {
  # Industry data from ACS Table S2403: Industry by Sex for the Civilian 
  # Employed Population 16 Years and Over
  # https://data.census.gov/table/ACSST5Y2022.S2401
  suppressMessages(get_acs_table("S2403", census_unit = census_unit)) |>
    process_nested_table() |>
    pivot_and_write(name = "ind_unit")
  
  suppressMessages(get_acs_table("S2403", census_unit = "place")) |>
    process_nested_table() |>
    process_places() |>
    pivot_and_write(name = "ind_place", unique_col = c("unit_id", "name"))
}