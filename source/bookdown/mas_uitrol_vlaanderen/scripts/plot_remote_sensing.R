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
    filter(pointid %in% loc_name) %>%
    # Create variable to set x-axis limits
    mutate(
      monthday_date = as.Date(
        paste0("2000-", substr(monthday, 1, 2), "-", substr(monthday, 3, 4))
      )
    )

  # Return simple plot
  if (is.null(shape_var)) {
    # Create output
    p <- data %>%
      ggplot(aes(x = monthday_date, y = !!sym(y_var), group = ref_id)) +
      # Create time series
      geom_line(alpha = 0.5, linetype = 5) +
      geom_point(aes(colour = !!sym(colour_var)), size = 2, show.legend = TRUE) +
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
    ggplot(aes(x = monthday_date, y = !!sym(y_var), group = ref_id)) +
    # Create time series
    geom_line(alpha = 0.5, linetype = 5) +
    geom_point(aes(colour = !!sym(colour_var), shape = !!sym(shape_var)),
               size = 2, show.legend = TRUE) +
    # Add reference lines
    geom_hline(yintercept = cutoff, linetype = "dotdash", colour = "black") +
    # Settings
    labs(x = "", y = y_axis_title, colour = legend_colour, shape = legend_shape) +
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
plot_trend_by_crop <- function(df) {
  legend_fill <- paste0(
    "Trend in bedekking (", toupper(unique(df$indicator)), ")"
  )

  df %>%
    group_by(year, gwsgrp_h) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    ggplot(aes(x = gwsgrp_h, fill = trend)) +
    geom_bar(position = "fill") +
    geom_text(aes(label = n, y = 1.05), size = 2.3) +
    labs(x = "", y = "Proportie percelen", fill = legend_fill) +
    facet_wrap(~year, ncol = 2) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 75, hjust = 1),
          strip.text = element_text(face = "bold"),
          legend.position = c(0.8, -0.4),
          legend.background = element_rect(fill = "white", color = "darkgrey"),
          legend.margin = margin(6, 6, 6, 6),
          legend.box = "horizontal",
          legend.text = element_text(size = 8))
}
