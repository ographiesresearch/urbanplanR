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
    geometry = geometry,
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
  df |>
    dplyr::rename(
      unit_id = GEOID
    )
}

get_acs_table <- function(table, 
                          states = CONFIG$states, 
                          year = CONFIG$year,
                          census_unit = CONFIG$census_unit,
                          var_match = "",
                          var_suffix = TRUE) {
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

get_acs_housing <- function(census_unit = CONFIG$census_unit) {
  vars <- c("hsg_unts" = "B25024_001",
            "oo" = "B25042_002",
            "oo0br" = "B25042_003",
            "oo1br" = "B25042_004",
            "oo2br" = "B25042_005",
            "oo3br" = "B25042_006",
            "oo4br" = "B25042_007",
            "oogt5br" = "B25042_008",
            "ro" = "B25042_009",
            "ro0br" = "B25042_010",
            "ro1br" = "B25042_011",
            "ro2br" = "B25042_012",
            "ro3br" = "B25042_013",
            "ro4br" = "B25042_014",
            "rogt5br" = "B25042_015",
            "unt1d" = "B25024_002",
            "unt1a" = "B25024_003",
            "unt2" = "B25024_004",
            "unt3_4" = "B25024_005",
            "unt5_9" = "B25024_006",
            "unt10_19" = "B25024_007",
            "unt20_49" = "B25024_008",
            "unt50up" = "B25024_009",
            "untmbl" = "B25024_010",
            "untbtrv" = "B25024_011",
            "untown" = "B25012_002",
            "untrnt" = "B25012_010",
            "mgr" = "B25031_001",
            "mgr0br" = "B25031_002",
            "mgr1br" = "B25031_003",
            "mgr2br" = "B25031_004",
            "mgr3br" = "B25031_005",
            "mgr4br" = "B25031_006",
            "mgrgt5br" = "B25031_007"
            )
  get_acs_vars(vars, census_unit = census_unit, geometry = FALSE)
}

get_acs_race <- function(census_unit = CONFIG$census_unit) {
  vars <- c(
    "white" = "B03002_003",
    "black" = "B03002_004",
    "hisp_lat" = "B03002_012",
    "indig" = "B03002_005",
    "asian" = "B03002_006",
    "hw_pi" = "B03002_007",
    "other" = "B03002_008",
    "multi" = "B03002_009",
    "multi" = "B03002_009",
    # PER-CAPITA INCOME
    # Non-Hispanic
    "wht_pci" = "B19301H_001",
    # Following include Hispanic
    "blk_pci" = "B19301B_001",
    "ind_pci" = "B19301C_001",
    "asn_pci" = "B19301D_001",
    # Not available at sub-county level
    # "hwp_pci" = "B19301E_001",
    "oth_pci" = "B19301F_001",
    "mti_pci" = "B19301G_001",
    "hslt_pci" = "B19301I_001",
    # MEDIAN HOUSEHOLD INCOME
    # Non-Hispanic
    "wht_mhi" = "B19013A_001",
    # Following include Hispanic
    "blk_mhi" = "B19013B_001",
    "ind_mhi" = "B19013C_001",
    "asn_mhi" = "B19013D_001",
    # Not available at sub-county level
    # "hwp_pci" = "B19301E_001",
    "oth_mhi" = "B19013F_001",
    "mti_mhi" = "B19013G_001",
    "hslt_mhi" = "B19013I_001"
  )
  get_acs_vars(vars, census_unit = census_unit, geometry = FALSE)
}

get_acs_age <- function(census_unit = CONFIG$census_unit) {
  vars <- c(
    "tot" = "B01001A_001",
    "mtot" = "B01001A_002",
    "mlt5" = "B01001A_003",
    "m5_9" = "B01001A_004",
    "m10_14" = "B01001A_005",
    "m15_17" = "B01001A_006",
    "m18_19" = "B01001A_007",
    "m20_24" = "B01001A_008",
    "m25_29" = "B01001A_009",
    "m30_34" = "B01001A_010",
    "m35_44" = "B01001A_011",
    "m45_54" = "B01001A_012",
    "m55_64" = "B01001A_013",
    "m65_74" = "B01001A_014",
    "m75_84" = "B01001A_015",
    "mgt85" = "B01001A_016",
    "ftot" = "B01001A_017",
    "flt5" = "B01001A_018",
    "f5_9" = "B01001A_019",
    "f10_14" = "B01001A_020",
    "f15_17" = "B01001A_021",
    "f18_19" = "B01001A_022",
    "f20_24" = "B01001A_023",
    "f25_29" = "B01001A_024",
    "f30_34" = "B01001A_025",
    "f35_44" = "B01001A_026",
    "f45_54" = "B01001A_027",
    "f55_64" = "B01001A_028",
    "f65_74" = "B01001A_029",
    "f75_84" = "B01001A_030",
    "fgt85" = "B01001A_031")
  get_acs_vars(vars, census_unit = census_unit, geometry = FALSE) |>
    dplyr::mutate(
      tlt5 = rowSums(dplyr::across(dplyr::matches("lt5")), na.rm = TRUE),
      t5_9 = rowSums(dplyr::across(dplyr::matches("5_9")), na.rm = TRUE),
      t10_14 = rowSums(dplyr::across(dplyr::matches("10_14")), na.rm = TRUE),
      t15_17 = rowSums(dplyr::across(dplyr::matches("15_17")), na.rm = TRUE),
      t18_19 = rowSums(dplyr::across(dplyr::matches("18_19")), na.rm = TRUE),
      t20_24 = rowSums(dplyr::across(dplyr::matches("20_24")), na.rm = TRUE),
      t25_29 = rowSums(dplyr::across(dplyr::matches("25_29")), na.rm = TRUE),
      t30_34 = rowSums(dplyr::across(dplyr::matches("30_34")), na.rm = TRUE),
      t35_44 = rowSums(dplyr::across(dplyr::matches("35_44")), na.rm = TRUE),
      t45_54 = rowSums(dplyr::across(dplyr::matches("45_54")), na.rm = TRUE),
      t55_64 = rowSums(dplyr::across(dplyr::matches("55_64")), na.rm = TRUE),
      t65_74 = rowSums(dplyr::across(dplyr::matches("65_74")), na.rm = TRUE),
      t75_84 = rowSums(dplyr::across(dplyr::matches("75_84")), na.rm = TRUE),
      tgt85 = rowSums(dplyr::across(dplyr::matches("gt85")), na.rm = TRUE),
      f5_17 = rowSums(dplyr::across(c(f5_9, f10_14, f15_17)), na.rm = TRUE),
      f18_24 = rowSums(dplyr::across(c(f18_19, f20_24)), na.rm = TRUE),
      f25_34 = rowSums(dplyr::across(c(f25_29, f30_34)), na.rm = TRUE),
      fgt65 = rowSums(dplyr::across(c(f65_74, f75_84, fgt85)), na.rm = TRUE),
      m5_17 = rowSums(dplyr::across(c(m5_9, m10_14, m15_17)), na.rm = TRUE),
      m18_24 = rowSums(dplyr::across(c(m18_19, m20_24)), na.rm = TRUE),
      m25_34 = rowSums(dplyr::across(c(m25_29, m30_34)), na.rm = TRUE),
      mgt65 = rowSums(dplyr::across(c(m65_74, m75_84, mgt85)), na.rm = TRUE)
    ) |>
    dplyr::select(
      unit_id,
      tidyselect::starts_with("t"), 
      tidyselect::starts_with("f"),
      tidyselect::starts_with("m")
    )
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