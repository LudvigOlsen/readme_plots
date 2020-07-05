
library(rearrr)
library(knitr)        # kable()
library(dplyr)        # %>% arrange()
library(tidyr)        # gather()
library(ggplot2)
# library(patchwork)
library(gganimate)

save_path <- "/Users/ludvigolsen/Documents/Programmering/readme_plots/rearrr/"

#   __________________ #< 63a9770bb959ffb368eaa2d544b61319 ># __________________
#   Rearrange examples                                                      ####


rearrange_gif <- function(){
  xpectr::set_test_seed(1)

  gg_line_alpha <- .4
  gg_base_line_size <- .3

  vec <- 1:11
  random_sample <- runif(11)
  orderings <- data.frame(
    "Position" = as.integer(vec),
    "Original" = vec,
    "center_max" = center_max(vec),
    "center_min" = center_min(vec),
    "position_max" = position_max(vec, position = 3),
    "position_min" = position_min(vec, position = 3),
    "pair_extremes" = pair_extremes(vec),
    "rev_windows" = rev_windows(vec, window_size = 3),
    "closest_to" = closest_to(vec, origin = 6),
    "furthest_from" = furthest_from(vec, origin = 3),
    stringsAsFactors = FALSE
  )

  # Convert to long format for plotting
  orderings <- orderings %>%
    tidyr::gather(key = "Method", value = "Value", 2:(ncol(orderings))) %>%
    dplyr::mutate(Method = factor(Method, levels = c(
      "Original", "center_max", "center_min",
      "position_max", "position_min",
      "closest_to", "furthest_from",
      "rev_windows", "pair_extremes"
    )),
                  Position = as.numeric(Position),
                  Value = as.numeric(Value)) %>%
    dplyr::as_tibble()

  orderings %>%
    ggplot(aes(x = Position, y = Value)) +
    geom_line(alpha = gg_line_alpha, color = "#008744") +
    geom_point(color = "#ffa700") +
    theme_minimal(base_line_size = gg_base_line_size) +
    scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
    scale_y_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
    theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 6, 0)),
      axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(6, 0, 0, 0))
    ) +
    transition_states(
      Method,
      transition_length = 2,
      state_length = 2
    ) +
    labs(title = "Rearranged: {closest_state}")

}

rearr_gif <- rearrange_gif()

# animate(rearr_gif, height = 3.5, width = 3.5, units = "in", res = 250, start_pause = 1)
#
# anim_save(paste0(save_path, "rearrange_example.gif"))



#   __________________ #< a52f3eedce80a224547c4a11b7fed53a ># __________________
#   Mutator examples                                                        ####

rotate2d_gif <- function(){
  xpectr::set_test_seed(1)

  gg_line_alpha <- .4
  gg_base_line_size <- .3

  vec <- 1:11
  random_sample <- runif(11)
  rotate_df <- rotate_2d(random_sample, degrees = as.integer(round(seq.default(from = 0, to = 359, by = 6))),
                         origin_fn = centroid) %>%
    dplyr::arrange(.degrees, Index)

  rotate_df %>%
    ggplot(aes(x = Index_rotated, y = Value_rotated)) +
    geom_hline(yintercept = mean(random_sample), size = 0.2, alpha = gg_line_alpha, linetype="dashed") +
    geom_vline(xintercept = mean(seq_len(length(random_sample))), size = 0.2, alpha = gg_line_alpha, linetype="dashed") +
    #geom_path(alpha = gg_line_alpha, color = "#008744") +
    geom_point(color = "#ffa700") +
    theme_minimal(base_line_size = gg_base_line_size) +
    scale_colour_brewer(palette = "Dark2") +
    theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 6, 0)),
      axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(6, 0, 0, 0))
    ) +
    transition_states(
      .degrees, state_length = 0
    ) +
    labs(x = "Index", y = "Value", title = "{closest_state} degrees")

}

rota2d_gif <- rotate2d_gif()

# animate(rota2d_gif, height = 3.5, width = 3.5, units = "in", res = 250)
#
# anim_save(paste0(save_path, "rotate2d_example.gif"))


##  ............................................................................
##  swirl                                                                   ####


swirl3d_gif <- function(){

  gg_line_alpha <- .4
  gg_base_line_size <- .3

  # Set seed
  set.seed(4)

  # Create a data frame
  df <- data.frame(
    "x" = 1:50,
    "y" = 1:50,
    "z" = 1:50,
    "r1" = runif(50),
    "r2" = runif(50) * 35,
    "o" = 1,
    "g" = rep(1:5, each=10)
  ) %>%
    dplyr::mutate(.radius_lev = 1)

  df_swirled_1 <- swirl_3d(
    data = df,
    x_col = "x",
    y_col = "y",
    z_col = "z",
    x_radius = c(0, 100, 0, 0),
    y_radius = c(0, 0, 100, 0),
    z_radius = c(0, 0, 0, 100),
    origin_fn = centroid,
    suffix = ""
  ) %>%
    dplyr::mutate(.radius_lev = as.numeric(factor(.radius_str)))

  df_swirled_2 <- swirl_3d(
    data = df,
    x_col = "x",
    y_col = "y",
    z_col = "z",
    x_radius = c(50, 0, 0, 100),
    y_radius = c(0, 50, 0, 100),
    z_radius = c(0, 0, 50, 100),
    origin_fn = centroid,
    suffix = ""
  ) %>%
    dplyr::mutate(.radius_lev = as.numeric(factor(.radius_str)))

  df_swirled_3 <- swirl_3d(
    data = df,
    x_col = "x",
    y_col = "y",
    z_col = "z",
    x_radius = c(25, 50, 25, 25),
    y_radius = c(50, 75, 100, 25),
    z_radius = c(75, 25, 25, 25),
    origin_fn = centroid,
    suffix = "",
    scale_fn = function(x) {
      x^0.81
    }
  ) %>%
    dplyr::mutate(.radius_lev = as.numeric(factor(.radius_str)))

  df_swirled_4 <- swirl_3d(
    data = df,
    x_col = "x",
    y_col = "y",
    z_col = "z",
    x_radius = c(25, 25, 0, 0),
    y_radius = c(25, 0, 25, 0),
    z_radius = c(0, 25, 25, 0),
    origin_fn = centroid,
    suffix = "",
    scale_fn = function(x) {
      x^0.75
    }
  ) %>%
    dplyr::mutate(.radius_lev = as.numeric(factor(.radius_str)))

  df_collected <- dplyr::bind_rows(
    df_swirled_1, df_swirled_2, df_swirled_3, df_swirled_4, .id = "version"
  ) %>%
    dplyr::mutate(.radius_lev = factor(.radius_lev))

  df_collected %>%
    ggplot(aes(x = x, y = y, color = .radius_lev)) +
    geom_path(alpha = gg_line_alpha) +
    geom_point() +
    theme_minimal(base_line_size = gg_base_line_size) +
    scale_colour_brewer(palette = "Dark2") +
    theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 6, 0)),
      axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(6, 0, 0, 0))
    ) +
    transition_states(
      version, state_length = 0
    ) +
    labs(x = "x", y = "y", color = "radius")

}

swirl_gif <- swirl3d_gif()

animate(swirl_gif, height = 3.5, width = 3.5, units = "in", res = 250, rewind = FALSE)

anim_save(paste0(save_path, "swirl3d_example.gif"))

