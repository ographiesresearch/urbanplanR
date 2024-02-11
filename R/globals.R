CONFIG <- jsonlite::read_json('config.json')
if ("census_api" %in% names(CONFIG)) {
  message("Census API key set.")
  suppressMessages(tidycensus::census_api_key(CONFIG$census_api))
} else {
  message("No census API key privided. Consider setting `census_api` in `config.json`.")
}

lehd_census_unit <- function() {
  if (CONFIG$census_unit %in% c("tract", "tracts")) {
    CONFIG$census_unit <<- "tract"
  } else if (CONFIG$census_unit %in% c("block groups", "block group", "bg")) {
    CONFIG$census_unit <<- "bg"
  } else {
    stop("census_unit parameter must be one of 'tracts' or 'block groups'.")
  }
  message(glue::glue("Census areal unit set to {CONFIG$census_unit}."))
}

lehd_census_unit()

options(
  # Suppress `summarise()` has grouped output by 'x'...'z' message.
  dplyr.summarise.inform = FALSE,
  # Suppress read/write CSV progress bar.
  readr.show_progress = FALSE
  )


STATE_LONG <- state.name[match(CONFIG$states,state.abb)]

std_format <- function() {
  if (CONFIG$format %in% c("shapefile", "shp")) {
    CONFIG$format <<- "shp"
  } else if (CONFIG$format %in% c("geopackage", "gpkg")) {
    CONFIG$format <<- "gpkg"
  } else if (CONFIG$format %in% c("geojson", "json")) {
    CONFIG$format <<- "geojson"
  } else {
    stop("'format' parameter must be one of 'shp', 'gpkg', or 'geojson'.")
  }
  message(glue::glue("Output format set to {CONFIG$format}."))
}

std_format()

tidy_census_units <- function() {
  if (CONFIG$census_unit %in% c("tract", "tracts")) {
    CONFIG$census_unit <<- "tract"
  } else if (CONFIG$census_unit %in% c("block groups", "block group", "bg")) {
    CONFIG$census_unit <<- "cbg"
  } else {
    stop("census_unit parameter must be one of 'tracts' or 'block groups'.")
  }
  message(glue::glue("Census areal unit set to {CONFIG$census_unit}."))
}

write_multi <- function(df, 
                        name, 
                        dir_name = CONFIG$project, 
                        format = CONFIG$format) {
  
  message(glue::glue("Writing {name}."))
  if (format == "gpkg") {
    sf::st_write(
      df,
      stringr::str_c(dir_name, format, sep="."),
      name,
      append = FALSE,
      delete_layer = TRUE,
      quiet = TRUE
    )
  } else {
    dir.create(dir_name, showWarnings = FALSE)
    if ("sf" %in% class(df)) {
      sf::st_write(
        df,
        file.path(
          dir_name,
          stringr::str_c(name, format, sep=".")
        ),
        append = FALSE,
        delete_dsn = TRUE,
        quiet = TRUE
      )
    } else {
      readr::write_csv(
        stringr::str_c(name, "csv", sep="."),
        append = FALSE
      )
    }
  }
}

get_remote_zip <- function(url, path) {
  httr::GET(
    paste0(url), 
    httr::write_disk(path, overwrite = TRUE)
  )
}

read_shp_from_zip <- function(path, layer) {
  path <- stringr::str_c("/vsizip/", path, "/", layer)
  sf::st_read(path, quiet=TRUE)
}

get_mass_munis <- function(crs = CONFIG$crs) {
  temp <- base::tempfile(fileext = ".zip")
  get_remote_zip(
    url = "https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/townssurvey_shp.zip",
    path = temp
  )
  read_shp_from_zip(temp, "TOWNSSURVEY_POLYM.shp") |>
    sf::st_transform(crs)
}

get_places <- function(states = CONFIG$states, 
                       year = CONFIG$year,
                       crs = CONFIG$crs) {
  place_geo <- tigris::places(
    state = states,
    year = year,
    cb = TRUE
  ) |>
    sf::st_transform(crs)
}

prep_places <- function(df) {
  df |>
    center_xy() |>
    dplyr::select(
      pl_id = GEOID,
      pl_name = NAME,
      x_pl = x,
      y_pl = y
    )
}

prep_munis <- function(df) {
  df |>
    center_xy() |>
    dplyr::mutate(
      town = stringr::str_to_title(TOWN)
    ) |>
    dplyr::select(
      pl_id = TOWN_ID,
      pl_name = town,
      x_pl = x,
      y_pl = y
    )
}

place_decision <- function(states = CONFIG$states) {
  if (states == "MA") {
    place_geo <- get_mass_munis() |>
      prep_munis()
  } else {
    place_geo <- get_places() |>
      prep_places()
  }
  place_geo
}

get_census_units <- function(states = CONFIG$states, 
                             year = CONFIG$year, 
                             crs = CONFIG$crs,
                             census_unit = CONFIG$census_unit) {
  if (census_unit == "tract") {
    message("Downloading tract geometries.")
    df <- tigris::tracts(
      year = year, 
      state = states, 
      cb = TRUE,
      progress_bar = FALSE
    )
  } else if (census_unit == "bg") {
    message("Downloading block group geometries.")
    df <- tigris::block_groups(
      year = year, 
      state = states, 
      cb = TRUE,
      progress_bar = FALSE
    )
  } else {
    stop("census_unit parameter must be one of 'tracts' or 'block groups'.")
  }
  df |>
    sf::st_transform(crs)
}
