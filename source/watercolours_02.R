library(tidyverse)

import_halftone <- function(base, channel, x_sample, y_sample) {

  channel_index <- channel %>%
    switch("red" = 1, "green" = 2, "blue" = 3)

  halftone_image <- base[, , 1, channel_index] %>%
    halftoner::halftone(x.samp = x_sample, y.samp = y_sample)

  return(tibble(
    x = halftone_image[[1]][,2],
    y = halftone_image[[1]][,1],
    size = halftone_image[[2]],
    shade = channel
  ))
}

import_image <- function(image, x_sample = 500, y_sample = 500) {

  base <- image %>%
    imager::load.image() %>%
    as.array()

  halftone_r <- import_halftone(base, "red", x_sample, y_sample)
  halftone_g <- import_halftone(base, "green", x_sample, y_sample)
  halftone_b <- import_halftone(base, "blue", x_sample, y_sample)

  merged_size <- (halftone_r$size + halftone_g$size + halftone_b$size)/3
  merged_shade <- rgb(
    red   = (1 - halftone_r$size),
    green = (1 - halftone_g$size),
    blue  = (1 - halftone_b$size),
    maxColorValue = 1.0
  )

  return(tibble(
    x = halftone_r$x / x_sample * 200,
    y = halftone_r$y / y_sample * 200,
    size = merged_size,
    shade = merged_shade
  ))
}

watercolours <- function(image, title = "", seed = 4,
                         resolutions =  c(500, 1000, 2000, 4000, 8000),
                         types = c(".png", ".jpg")) {


  image_path <- fs::path("input", paste0("input_", image, ".jpg"))
  build_msg <- paste("building from", image_path)
  message(build_msg)

  ht <- import_image(here::here(image_path))

  min_size <- 0
  max_size <- .5

  set.seed(seed)

  ht <- ht %>%
    filter(size > min_size) %>%
    mutate(size = if_else(size > max_size, .5, size)) %>%
    mutate(
      id = 1,
      seed = seed,
      ind = 1:n(),
      type = "halftone"
    )

  its <- 80

  dat <- ht %>%
    mutate(
      x = x * .01,
      y = y * .01
    ) %>%
    jasmines::unfold_breeze(
      iterations = its,
      drift = 0,
      scale = .0001,
      octaves = 10
    )

  compute_limit <- function(data, column, border = .04) {
    values <- data[[column]][data$time == 1]
    range <- c(1 - max(values), 1 - min(values))
    limit <- range + c(1, -1) * border
    return(limit)
  }

  size_decay <- function(size, time) {
    size * 8 * abs((its - time)/its)
  }

  pic <- ggplot(dat) +
    geom_point(
      mapping = aes(
        x = 1 - x,
        y = 1 - y,
        color = shade,
        size = size_decay(size, time)
      ),
      alpha = 1,
      stroke = 0,
      show.legend = FALSE
    ) +
    coord_cartesian(
      xlim = compute_limit(dat, "x"),
      ylim = compute_limit(dat, "y")
    ) +
    scale_x_continuous(name = NULL, expand = c(0,0), breaks = NULL) +
    scale_y_continuous(name = NULL, expand = c(0,0), breaks = NULL) +
    scale_size_identity() +
    scale_colour_identity() +
    scale_alpha_continuous(range = c(0, 1)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white"))

  # ugh
  rm(ht, dat)
  gc()

  if(nchar(title) > 0) title <- paste0("_", title)
  output <- paste0("watercolour_sys02_img", image, title)
  scaling <- 40/3;

  for(size in resolutions) {
    for(type in types) {
      output_path <- fs::path("docs", size, paste0(output, type))
      build_msg <- paste("making", output_path)
      message(build_msg)
      ggsave(
        filename = here::here(output_path),
        plot = pic,
        width = scaling,
        height = scaling,
        dpi = size / scaling
      )
    }
  }
}



