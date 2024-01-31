get_acs_table <- function(table, 
                          states = CONFIG$states, 
                          year = CONFIG$year,
                          census_unit = CONFIG$census_unit) {
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
      variable = stringr::str_c(
        stringr::str_c(
          "B",
          stringr::str_sub(table, 2),
          "1",
          sep = ""
        ),
        stringr::str_extract(variable, "(?<=_)\\d+$"),
        sep="_"
      )
    ) |>
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
        "(?<=!!)[0-9A-Za-z\\s,]+(?=(?:$|:$))"
      ),
      levels_flag = dplyr::case_when(
        (level == dplyr::lead(level)) ~ TRUE,
        (level == dplyr::lag(level)) & (level > dplyr::lead(level)) ~ TRUE,
        .default = FALSE
      )
    )
  
  max_level <- max(df$level)
  
  df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      levels = dplyr::case_when(
        levels_flag ~ list(seq.int(level, max_level, 1)),
        .default = list(level)
      )
    )
}

process_places <- function(df) {
  df |>
    dplyr::mutate(
      city = stringr::str_detect(
        NAME, "city,"
      ),
      NAME = stringr::str_extract(
        NAME,
        glue::glue(".*(?=((CDP|city), {STATE_LONG}$))")
      )
    )
}

pct_transform <- function(df, unique_col) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(unique_col))) |>
    dplyr::mutate(
      pct = estimate / max(estimate) * 100
    ) |>
    dplyr::ungroup()
}

pivot_and_write <- function(df, name, unique_col) {
  depths <- unique(df$level)
  depths <- depths[ !depths == 0]
  
  df <- df |>
    pct_transform(unique_col)
  
  for (d in depths) {
    df |>
      dplyr::rowwise() |>
      dplyr::filter(d %in% levels) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(unique_col),
        names_from = "label",
        values_from = "pct"
      ) |>
      write_multi(glue::glue("{name}_depth_{d}"))
  }
}

get_occupations <- function() {
  # Industry data from ACS Table S2401: Occupation by Sex for the Civilian 
  # Employed Population 16 Years and Over
  # https://data.census.gov/table/ACSST5Y2022.S2401
  suppressMessages(get_acs_table("S2401")) |>
    pivot_and_write(name = "occ", unique_col = "GEOID")
  
  suppressMessages(get_acs_table("S2401", census_unit = "place")) |>
    process_places() |>
    pivot_and_write(name = "occ_place", unique_col = "NAME")
}

get_industries <- function() {
  # Industry data from ACS Table S2403: Industry by Sex for the Civilian 
  # Employed Population 16 Years and Over
  # https://data.census.gov/table/ACSST5Y2022.S2401
  suppressMessages(get_acs_table("S2403", census_unit = "place")) |>
    process_places() |>
    pivot_and_write(name = "ind", unique_col = "GEOID")
  
  suppressMessages(get_acs_table("S2403", census_unit = "place")) |>
    process_places() |>
    pivot_and_write(name = "ind_place", unique_col = "NAME")
}