library(magrittr)

paths <- here::here("docs") %>%
  list.files(recursive = TRUE) %>%
  stringr::str_subset("jpg$|png$")

manifest <- tibble::tibble(path = paths) %>%
  tidyr::separate(
    col = path,
    into = c("resolution", "filename"),
    sep = "/",
    remove = FALSE
  ) %>%
  tidyr::separate(
    col = filename,
    into = c("filespec", "format"),
    sep = "\\."
  ) %>%
  tidyr::separate(
    col = filespec,
    into = c("series", "sys_id", "img_id", "short_name"),
    sep = "_"
  ) %>%
  dplyr::mutate(
    resolution = as.integer(resolution),
    long_name = dplyr::case_when(
      short_name == "incursions" ~ "Incursions",
      short_name == "inferno" ~ "Seventh Circle",
      short_name == "percolate" ~ "Percolate",
      short_name == "teacup-ocean" ~ "Ocean in a Teacup",
      short_name == "gentle-descent" ~ "Gentle Descent",
      short_name == "stormy-seas" ~ "Stormy Seas",
      short_name == "turmeric" ~ "Turmeric Against Grey Tuesday",
      short_name == "torn-and-frayed" ~ "Torn and Frayed",
      short_name == "storm-cell" ~ "Storm Cell / Air Elemental",
      short_name == "tonal-earth" ~ "Tonal Earth",
      short_name == "cold-front" ~ "Cold Front",
      short_name == "kintsugi-dreams" ~ "Kintsugi Dreams",
      short_name == "departure" ~ "Departure",
      short_name == "echo" ~ "Echo",
      TRUE ~ NA_character_
    )
  )

readr::write_csv(manifest, here::here("docs", "manifest.csv"))
