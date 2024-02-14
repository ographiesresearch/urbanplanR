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

get_from_arc <- function(dataset, crs = CONFIG$crs) {
  prefix <- "https://opendata.arcgis.com/api/v3/datasets/"
  suffix <- "/downloads/data?format=geojson&spatialRefId=4326&where=1=1"
  sf::st_read(
    glue::glue("{prefix}{dataset}{suffix}")
    ) |>
    dplyr::rename_with(tolower) |>
    sf::st_transform(crs)
}


get_ma_munis <- function(crs = CONFIG$crs) {
  message("Downloading Massachusetts municipal boundaries...")
  get_from_arc("43664de869ca4b06a322c429473c65e5_0") |>
    dplyr::mutate(
      town = stringr::str_to_title(town),
      state = "MA"
    ) |>
    dplyr::select(pl_name = town, state)
}

# Deprecated solution for downloading zipped shapefile from MassGIS.
# get_ma_munis <- function(crs = CONFIG$crs) {
#   message("Downloading Massachusetts municipal boundaries...")
#   temp <- base::tempfile(fileext = ".zip")
#   get_remote_zip(
#     url = "https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/state/townssurvey_shp.zip",
#     path = temp
#   )
#   read_shp_from_zip(temp, "TOWNSSURVEY_POLYM.shp") |>
#     dplyr::rename_with(tolower) |>
#     dplyr::mutate(
#       town_id = as.character(town_id),
#       town = stringr::str_to_title(town),
#       state = "MA"
#     ) |>
#     sf::st_transform(crs) |>
#     dplyr::select(pl_id = town_id, pl_name = town, state)
# }

get_me_munis <- function(crs = CONFIG$crs) {
  message("Downloading Maine municipal boundaries...")
  get_from_arc("289a91e826fd4f518debdd824d5dd16d_0") |>
    dplyr::filter(
      town != " "
    ) |>
    dplyr::mutate(
      state = "ME"
    ) |>
    sf::st_make_valid() |> 
    dplyr::group_by(pl_name = town, state) |>
    dplyr::summarize(
      geometry = sf::st_union(geometry)
    )
}

get_nh_munis <- function(crs = CONFIG$crs) {
  message("Downloading New Hampshire municipal boundaries...")
  get_from_arc("4edf75ab263b4d92996f92fb9cf435fa_8") |>
    dplyr::filter(
      pbpname != " "
    ) |>
    dplyr::mutate(
      state = "NH"
    ) |>
    dplyr::select(pl_name = pbpname, state)
}

get_vt_munis <- function(crs = CONFIG$crs) {
  message("Downloading Vermont municipal boundaries...")
  get_from_arc("3f464b0e1980450e9026430a635bff0a_0") |>
    dplyr::filter(
      townnamemc != " "
    ) |>
    dplyr::mutate(
      state = "VT"
    ) |>
    dplyr::select(pl_name = townnamemc, state)
}

get_ct_munis <- function(crs = CONFIG$crs) {
  message("Downloading Connecticut municipal boundaries...")
  get_from_arc("df1f6d681b7e41dca8bdd03fc9ae0dd6_1") |>
    dplyr::filter(
      town != " ", town != ""
    ) |>
    dplyr::mutate(
      state = "CT"
    ) |>
    dplyr::group_by(pl_name = town, state) |>
    dplyr::summarize(
      geometry = sf::st_union(geometry)
    ) |>
    dplyr::ungroup()
}

get_ri_munis <- function(crs = CONFIG$crs) {
  message("Downloading Rhode Island municipal boundaries...")
  get_from_arc("957468e8bb3245e8b3321a7bf3b6d4aa_0") |>
    dplyr::filter(
      name != " ", name != ""
    ) |>
    dplyr::mutate(
      name = stringr::str_to_title(name),
      state = "RI"
    ) |>
    dplyr::group_by(pl_name = name, state) |>
    dplyr::summarize(
      geometry = sf::st_union(geometry)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      pl_name, state
    )
}

get_places <- function(states = CONFIG$states, 
                       year = CONFIG$year,
                       crs = CONFIG$crs) {
  place_geo <- tigris::places(
      state = states,
      year = year,
      cb = TRUE
    ) |>
    sf::st_transform(crs) |>
    dplyr::rename_with(tolower) |>
    dplyr::select(
      pl_name = name,
      state = stusps
    )
}

center_xy <- function(sdf) {
  sdf |>
    dplyr::mutate(
      point = sf::st_point_on_surface(geometry),
      x = sf::st_coordinates(point)[,1],
      y = sf::st_coordinates(point)[,2]
    ) |>
    dplyr::select(-point)
}

prep_munis <- function(df) {
  df |>
    center_xy() |>
    dplyr::select(
      pl_name,
      state,
      x_pl = x,
      y_pl = y
    ) |>
    dplyr::mutate(
      pl_id = stringr::str_c(
        stringr::str_to_lower(pl_name), 
        stringr::str_to_lower(state),
        sep = "_"
        )
    )
}

place_decision <- function(states = CONFIG$states) {
  state_munis <- list()
  no_muni_st <- c()
  for (state in states) {
    if (state == "MA") {
      state_munis[[state]] <- get_ma_munis()
    } else if (state == "ME") {
      state_munis[[state]] <- get_me_munis()
    } else if (state == "NH") {
      state_munis[[state]] <- get_nh_munis()
    } else if (state == "VT") {
      state_munis[[state]] <- get_vt_munis()
    } else if (state == "CT") {
      state_munis[[state]] <- get_ct_munis()
    } else if (state == "RI") {
      state_munis[[state]] <- get_ri_munis()
    } else {
      no_muni_st <- append(no_muni_st, state)
    }
  }
  if (length(no_muni_st) > 0) {
    state_munis[["Other"]] <- get_places(states = no_muni_st)
  }
  dplyr::bind_rows(state_munis) |>
    prep_munis()
}

remove_coords <- function(df) {
  df |>
    dplyr::select(-dplyr::starts_with(c("x", "y")))
}

get_census_units <- function(states = CONFIG$states, 
                             year = CONFIG$year, 
                             crs = CONFIG$crs,
                             census_unit = CONFIG$census_unit,
                             cb = TRUE) {
  unit_container <- list()
  for (st in states) {
    if (census_unit == "tract") {
      message("Downloading tract geometries.")
      df <- tigris::tracts(
        year = year, 
        state = st, 
        cb = TRUE,
        progress_bar = FALSE
      )
    } else if (census_unit == "bg") {
      message("Downloading block group geometries.")
      df <- tigris::block_groups(
        year = year, 
        state = st, 
        cb = TRUE,
        progress_bar = FALSE
      )
    } else {
      stop("census_unit parameter must be one of 'tracts' or 'block groups'.")
    }
    unit_container[[st]] <- df
  }
  unit_container |>
    dplyr::bind_rows() |>
    sf::st_transform(crs) |>
    dplyr::rename_with(tolower) |>
    dplyr::rename(
      unit_id = geoid
    ) |>
    dplyr::select(
      unit_id
    )
}