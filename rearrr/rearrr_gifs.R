
library(rearrr)
library(knitr)        # kable()
library(dplyr)        # %>% arrange()
library(tidyr)        # gather()
library(ggplot2)
# library(patchwork)
library(gganimate)


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

animate(rearr_gif, height = 3.5, width = 3.5, units = "in", res = 250, start_pause = 1)

anim_save("rearrange_example.gif")
