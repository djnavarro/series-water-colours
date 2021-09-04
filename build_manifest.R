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
  dplyr::mutate(resolution = as.integer(resolution))

extras <- tibble::tribble(
  ~short_name, ~long_name, ~date,
  "incursions", "Incursions", "2021-08-14",
  "inferno", "Seventh Circle", "2021-08-27",
  "percolate", "Percolate", "2021-08-21",
  "teacup-ocean", "Ocean in a Teacup", "2021-07-31",
  "gentle-descent", "Gentle Descent", "2021-08-21",
  "stormy-seas", "Stormy Seas", "2021-08-22",
  "turmeric", "Turmeric Against Grey Tuesday", "2021-08-24",
  "torn-and-frayed", "Torn and Frayed", "2021-08-24",
  "storm-cell", "Storm Cell / Air Elemental", "2021-08-27",
  "tonal-earth", "Tonal Earth", "2021-08-29",
  "cold-front", "Cold Front", "2021-08-29",
  "kintsugi-dreams", "Kintsugi Dreams", "2021-08-29",
  "departure", "Departure", "2021-08-29",
  "echo", "Echo", "2021-08-30",
  "portal", "Portal", "2021-08-31",
  "salt-stone-storm", "Gods of Salt, Stone, and Storm", "2021-08-31",
  "amanecer-de-invierno", "El Último Amanecer de Invierno", "2021-09-01",
  "woodland-spirits", "Woodland Spirits", "2021-09-02",
  "plume", "Plume", "2021-09-02",
  "below-the-horizon", "Below the Horizon", "2021-09-03"
)

manifest <- manifest %>%
  dplyr::left_join(extras) %>%
  dplyr::arrange(date, img_id)

readr::write_csv(manifest, here::here("docs", "manifest.csv"))
