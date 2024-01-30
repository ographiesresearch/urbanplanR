CONFIG <- jsonlite::read_json('config.json')

STATE_LONG <- state.name[match(CONFIG$states,state.abb)]

# Load ACS variables.
VARS <- tidycensus::load_variables(
    year = CONFIG$year,
    dataset = "acs5",
    cache = TRUE
  ) |>
  dplyr::select(name, label)

load_acs <- function(table) {
  tidycensus::get_acs(
    geography = CONFIG$geography,
    table = table, 
    year = CONFIG$year,
    state = CONFIG$states,
    geometry = FALSE
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
      VARS, 
      by = c("variable" = "name")
    ) |>
    dplyr::mutate(
      city = stringr::str_detect(
        NAME, "city,"
      ),
      NAME = stringr::str_extract(
        NAME,
        glue::glue(".*(?=((CDP|city), {STATE_LONG}$))")
      ),
      level = stringr::str_count(label, "!!"),
      label = stringr::str_extract(
        label, 
        "(?<=!!)[0-9A-Za-z\\s,]+(?=(?:$|:$))"
        )
    )
}

pct_transform <- function(df) {
  df |>
    dplyr::group_by(NAME) |>
    dplyr::mutate(
      pct = estimate / max(estimate) * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(pct < 100)
}

# Industry data from ACS Table S2401: Occupation by Sex for the Civilian Employed
# Population 16 Years and Over
# https://data.census.gov/table/ACSST5Y2022.S2401?q=workforce&t=Occupation
occ <- load_acs("S2401")

# Industry data from ACS Table S2403: Industry by Sex for the Civilian Employed
# Population 16 Years and Over
# https://data.census.gov/table/ACSST5Y2022.S2403?q=workforce&t=Occupation
ind <- load_acs("S2403")

ind |>
  pct_transform() |>
  dplyr::filter(level == 2 & city) |>
  plotly::plot_ly(
    x = ~ pct,
    y = ~ NAME,
    type = 'bar',
    color = ~ label,
    name = ~ label
    ) |>
  plotly::layout(yaxis = list(title = 'Cities'), barmode = 'stack') 


# |>
#   tidyr::pivot_wider(
#     id_cols = c("GEOID", "NAME"),
#     names_from = "label",
#     values_from = "estimate"
#   )

# geom <- tigris::places(
#     state = CONFIG$states,
#     year = CONFIG$year,
#     cb = TRUE
#   ) |>
#   dplyr::left_join(
#     occupation, 
#     by = c("GEOID" = "GEOID")
#   )
