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
      TRUE ~ NA_character_
    )
  )

readr::write_csv(manifest, here::here("docs", "manifest.csv"))
