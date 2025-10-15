#' Plot indicator time series for a specific plot
#'
#' Creates a time series plot of NDVI or BSI values for one or more selected
#' plots, optionally distinguishing points by colour and/or shape. The function
#' automatically determines whether to plot NDVI or BSI based on column names
#' and adds reference lines for the indicator threshold.
#'
#' @param df A data frame containing time series data, including columns for
#' `pointid`, `monthday`, `year`, and either `ndvi` or `bsi`.
#' @param loc_name Character vector specifying one or more plot IDs (`pointid`)
#' to include.
#' @param colour_var Name of the variable used to colour points.
#' @param shape_var Optional; name of the variable used to vary point shapes.
#' Defaults to `NULL`.
#' @param legend_colour Character string for the legend title of the colour
#' scale.
#' @param legend_shape Character string for the legend title of the shape scale.
plot_time_series <- function(
  df,
  loc_name,
  colour_var,
  shape_var = NULL,
  legend_colour = "Legende",
  legend_shape = "Legende"
) {
  require("dplyr")
  require("ggplot2")
  require("rlang")

  # Rules for different indicator types
  if ("ndvi" %in% names(df)) {
    y_axis_title <- "NDVI"
    y_var <- "ndvi"
    cutoff <- 0.3
  } else {
    y_axis_title <- "BSI"
    y_var <- "bsi"
    cutoff <- 0.021
  }

  # Prepare data
  data <- df %>%
    # Get plot
    filter(.data$pointid %in% loc_name) %>%
    # Create variable to set x-axis limits
    mutate(
      monthday_date = as.Date(
        paste0("2000-", substr(.data$monthday, 1, 2), "-",
               substr(.data$monthday, 3, 4))
      )
    )

  # Return simple plot
  if (is.null(shape_var)) {
    # Create output
    p <- data %>%
      ggplot(aes(x = .data$monthday_date, y = !!sym(y_var),
                 group = .data$ref_id)) +
      # Create time series
      geom_line(alpha = 0.5, linetype = 5) +
      geom_point(aes(colour = !!sym(colour_var)), size = 2,
                 show.legend = TRUE) +
      # Add reference lines
      geom_hline(yintercept = cutoff, linetype = "dotdash", colour = "black") +
      # Settings
      labs(x = "", y = y_axis_title, colour = legend_colour) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b",
                   limits = as.Date(c("2000-02-01", "2000-09-01"))) +
      scale_colour_discrete(drop = FALSE) +
      facet_wrap(~year, ncol = 1, scales = "fixed")
    return(p)
  }

  # Return complex plot
  data %>%
    ggplot(aes(x = .data$monthday_date, y = !!sym(y_var),
               group = .data$ref_id)) +
    # Create time series
    geom_line(alpha = 0.5, linetype = 5) +
    geom_point(aes(colour = !!sym(colour_var), shape = !!sym(shape_var)),
               size = 2, show.legend = TRUE) +
    # Add reference lines
    geom_hline(yintercept = cutoff, linetype = "dotdash", colour = "black") +
    # Settings
    labs(x = "", y = y_axis_title, colour = legend_colour,
         shape = legend_shape) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 limits = as.Date(c("2000-02-01", "2000-09-01"))) +
    scale_shape_manual(
      values = c("bedekt" = 16, "onbedekt" = 4),
      drop = FALSE
    ) +
    scale_colour_discrete(drop = FALSE) +
    facet_wrap(~year, ncol = 1, scales = "fixed")
}


#' Plot distribution of indicator trends by crop type
#'
#' Creates a bar plot showing the proportion of plots with different indicator
#' trends (e.g. increase, decrease, stable) per crop type and year. Bar heights
#' are expressed as proportions within each crop group, and counts are displayed
#' above the bars.
#'
#' @param df A data frame containing columns for `year`, `gwsgrp_h`
#' (crop group), `trend`, and `indicator`.
plot_trendtype_by_crop <- function(df) {
  require("dplyr")
  require("ggplot2")
  require("rlang")

  legend_fill <- paste0(
    "Trend in bedekking (", toupper(unique(df$indicator)), ")"
  )

  df %>%
    group_by(.data$year, .data$gwsgrp_h) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    ggplot(aes(x = .data$gwsgrp_h, fill = .data$trend)) +
    geom_bar(position = "fill") +
    geom_text(aes(label = n, y = 1.05), size = 2.3) +
    labs(x = "", y = "Proportie percelen", fill = legend_fill) +
    facet_wrap(~year, ncol = 2) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 75, hjust = 1),
          strip.text = element_text(face = "bold"),
          legend.position = c(0.8, -0.42),
          legend.background = element_rect(fill = "white", color = "darkgrey"),
          legend.margin = margin(6, 6, 6, 6),
          legend.box = "horizontal",
          legend.text = element_text(size = 8))
}


#' Plot seasonal trends by crop group
#'
#' Generates a facetted time series plot of either NDVI or BSI values per crop
#' group (`gwsgrp_h`) and year. The plot shows the central tendency (default
#' median) with an interquartile or custom quantile ribbon, highlights points
#' above/below a threshold, and formats long crop group names for readability.
#'
#' @param df A data frame containing at least the columns `gwsgrp_h`, `year`,
#' `monthday`, and either `ndvi` or `bsi`.
#' @param order_levels A character vector specifying the desired order of
#' `gwsgrp_h` categories for plotting.
#' @param .f A summary function to calculate the central tendency (default is
#' `median`).
#' @param prob Numeric value between 0 and 0.5 specifying the quantile for the
#' lower and upper bounds of the ribbon (default 0.25).

plot_trend_by_crop <- function(df, order_levels, .f = median, prob = 0.25) {
  require("dplyr")
  require("ggplot2")
  require("rlang")

  # Rules for different indicator types
  if ("ndvi" %in% names(df)) {
    y_axis_title <- "NDVI"
    y_var <- "ndvi"
    cutoff <- 0.3
    rule <- `<`
  } else {
    y_axis_title <- "BSI"
    y_var <- "bsi"
    cutoff <- 0.021
    rule <- `>`
  }

  # Create data
  plot_summary <- df %>%
    mutate(
      monthday_date = as.Date(
        paste0("2000-", substr(.data$monthday, 1, 2), "-",
               substr(.data$monthday, 3, 4))
      )
    ) %>%
    group_by(.data$gwsgrp_h, .data$year, .data$monthday_date) %>%
    summarise(
      center_val = .f(.data[[y_var]], na.rm = TRUE),
      q_low = quantile(.data[[y_var]], prob, na.rm = TRUE),
      q_high = quantile(.data[[y_var]], 1 - prob, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Sort gwsgrp_h according to trend_data
    mutate(
      gwsgrp_h = factor(.data$gwsgrp_h, levels = order_levels),
      cover = ifelse(exec(rule, .data$center_val, cutoff),
                     "onbedekt", "bedekt"),
      # Insert line break at the third space
      gwsgrp_h_short = gsub(
        pattern = "^((?:\\S+\\s){3})",  # match first two spaces
        replacement = "\\1\n",
        x = as.character(.data$gwsgrp_h)
      )
    )

  # Ensure the order of the short names matches the original gwsgrp_h order
  levels_short <- plot_summary %>%
    distinct(.data$gwsgrp_h, .data$gwsgrp_h_short) %>%
    arrange(factor(.data$gwsgrp_h, levels = order_levels)) %>%
    pull(.data$gwsgrp_h_short)

  plot_summary <- plot_summary %>%
    mutate(gwsgrp_h_short = factor(.data$gwsgrp_h_short, levels = levels_short))

  # Calculate trends
  trend_df <- plot_summary %>%
    arrange(.data$gwsgrp_h_short, .data$year, .data$monthday_date) %>%
    group_by(.data$gwsgrp_h_short, .data$year) %>%
    summarise(
      trend_raw = paste0(rle(cover)$values, collapse = " → "),
      trend = factor(
        case_when(
          trend_raw == "onbedekt → bedekt" ~ "toename bedekking",
          trend_raw == "bedekt → onbedekt" ~ "afname bedekking",
          trend_raw == "onbedekt → bedekt → onbedekt" ~ "parabool",
          trend_raw == "bedekt → onbedekt → bedekt" ~ "omgekeerde parabool",
          trend_raw == "bedekt" ~ "altijd bedekt",
          trend_raw == "onbedekt" ~ "altijd onbedekt",
          TRUE ~ "complex patroon"
        ),
        levels = c("altijd bedekt", "altijd onbedekt", "toename bedekking",
                   "afname bedekking", "parabool", "omgekeerde parabool",
                   "complex patroon")
      ),
      .groups = "drop"
    ) %>%
    select("year", "gwsgrp_h_short", "trend")

  # Position of trend lables
  trend_pos <- plot_summary %>%
    group_by(.data$gwsgrp_h_short) %>%
    summarise(
      y_pos = max(.data$center_val, na.rm = TRUE) + 0.1,
      .groups = "drop"
    ) %>%
    left_join(trend_df, by = "gwsgrp_h_short")

  # Create plot
  ggplot(plot_summary,
         aes(x = .data$monthday_date, group = .data$gwsgrp_h_short)) +
    geom_ribbon(aes(ymin = .data$q_low, ymax = .data$q_high), alpha = 0.2) +
    geom_line(aes(y = .data$center_val)) +
    geom_point(aes(y = .data$center_val, colour = .data$cover)) +
    geom_hline(yintercept = cutoff, linetype = "dotdash", colour = "black") +
    geom_text(
      data = trend_pos,
      aes(x = as.Date("2000-05-01"), y = .data$y_pos, label = .data$trend),
      hjust = 0.5, vjust = 0, size = 2,
      colour = "black"
    ) +
    scale_colour_manual(
      values = c("bedekt" = "#4CAF50", "onbedekt" = "#FFC107")
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 limits = as.Date(c("2000-02-01", "2000-09-01"))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
    facet_grid(gwsgrp_h_short ~ year, scales = "free_y") +
    labs(x = "", y = y_axis_title, colour = "Bedekkingstoestand") +
    theme_minimal(base_size = 12) +
    theme(strip.text.x = element_text(face = "bold"),
          strip.text.y = element_text(face = "bold", size = 6),
          legend.position = "bottom")
}
