#'  Plot detection functions in ggplot
#'
#' Various functions used to specify key and adjustment functions for
#' detection functions from mrds package.
#' Final function to plot the detection function with ggplot
#'
#' \code{scalevalue} for either detection function it computes the scale with
#' the log link using the parameters and the covariate design matrix.
#' From mrds:::scalevalue
#'
#' \code{keyfct_hz} calculates the detection probability according to a
#' hazard rate model.
#' From mrds:::keyfct.hz
#'
#' \code{keyfct_hn} calculates the detection probability according to a
#' half normal model.
#' From mrds:::keyfct.hn
#'
#' \code{plot_detection_curve} will create a bar graph of distances with
#' superimposed detection curves and average fitting curve according to a
#' dsmodel. In a multicovariate model, a design matrix and labels can be passed
#' to create detection functions for each category
#'
#' scalevalue(key_scale, z)
#'
#' keyfct_hn(distance, key_scale)
#'
#' keyfct_hz(distance, key_scale, key_shape)
#'
#' plot_detection_curve(
#'   ds_model,
#'   n_breaks = NULL,
#'   show_data = TRUE,
#'   plot_average_fit = TRUE,
#'   plot_covariate_fit = FALSE
#' )
#'
#' @param distance  vector of distances
#' @param z design matrix for scale function
#' @param key_scale vector of scale values
#' @param key_shape vector of shape values
#' @param ds_model dsmodel
#' @param n_breaks number of breaks for the bar graph
#' @param show_data logical used to indicate whether the distance data should be
#' plotted (default is TRUE)
#' @param plot_average_fit logical used to indicate whether the fitted average
#' detection curve should be plotted (default is TRUE)
#' @param plot_covariate_fit logical used to indicate whether the covariate
#' specific detection curves should be plotted (default is FALSE)
#'
#' @return
#' For \code{scalevalue}, vector of the scale parameters
#' For \code{keyfct_*}, vector of key function evaluations
#' For \code{plot_detection_curve}, ggplot object


scalevalue <- function(key_scale, z) {
  return(exp(as.matrix(z) %*% key_scale))
}


keyfct_hz <- function(distance, key_scale, key_shape) {
  return(1 - exp(-(distance / key_scale)^(-key_shape)))
}


keyfct_hn <- function(distance, key_scale) {
  return(exp(-((distance / (sqrt(2) * key_scale))^2)))
}

plot_detection_curve <- function(# nolint: cyclocomp_linter.
  ds_model,
  n_breaks = NULL,
  show_data = TRUE,
  plot_average_fit = TRUE,
  plot_covariate_fit = FALSE
) {
  require("mrds")
  require("dplyr")
  require("tidyr")
  require("ggplot2")

  if (!ds_model$ddf$meta.data$point) {
    stop(paste0("Function only works for point transects!"), call. = FALSE)
  }

  # Vector of distances used to re-create the detection function (from 0 out to
  # truncation distance)
  trunc <- ds_model$ddf$meta.data$width
  distances <- seq(0, trunc, length.out = 100)

  # dist_data is the data.frame object of the distance data
  dist_data <- ds_model$ddf$data

  #####
  # Calculate detection curves for different covariate combinations
  #####

  if (!ds_model$ddf$ds$aux$ddfobj$intercept.only && plot_covariate_fit) {
    # Make the design matrix
    mm <- ds_model$ddf$ds$aux$ddfobj$scale$dm
    vars <- all.vars(as.formula(ds_model$ddf$ds$aux$ddfobj$scale$formula))
    rownames(mm) <- apply(dist_data[vars], 1, paste, collapse = " - ")

    design_mat <- distinct(as_tibble(mm, rownames = "rownames")) %>%
      column_to_rownames("rownames") %>%
      as.data.frame()

    labels <- rownames(design_mat)

    key <- ds_model$ddf$ds$aux$ddfobj$type
    if (key == "hr") {
      # Shape parameter
      key_shape_hr <- scalevalue(ds_model$ddf$ds$aux$ddfobj$shape$parameters,
                                 matrix(1, nrow = 100, 1))

      # Scale parameter
      scaled_values <- scalevalue(ds_model$ddf$ds$aux$ddfobj$scale$parameters,
                                  design_mat) %>%
        as.data.frame() %>%
        mutate(cat = labels) %>%
        slice(rep(seq_len(n()), each = 100)) %>%
        group_by(cat) %>%
        group_split()

      # Calculate detection probability values
      y_vals <- lapply(scaled_values, function(x) {
        keyfct_hz(distances, x[, 1], key_shape_hr)
      })

      # Combine in dataframe
      df_y_val <- y_vals %>%
        data.frame() %>%
        `colnames<-`(sort(labels)) %>%
        pivot_longer(col = everything(), names_to = "Legend",
                     values_to = "y_val") %>%
        arrange(.data$Legend, desc(.data$y_val)) %>%
        mutate(dist = rep(distances, length(labels)))

    } else if (key == "hn") {
      # Scale parameter
      scaled_values <- scalevalue(ds_model$ddf$ds$aux$ddfobj$scale$parameters,
                                  design_mat) %>%
        as.data.frame() %>%
        mutate(cat = labels) %>%
        slice(rep(seq_len(n()), each = 100)) %>%
        group_by(cat) %>%
        group_split()

      # Calculate detection probability values
      y_vals <- lapply(scaled_values, function(x) {
        keyfct_hn(distances, x[, 1])
      })

      # Combine in dataframe
      df_y_val <- y_vals %>%
        data.frame() %>%
        `colnames<-`(sort(labels)) %>%
        pivot_longer(col = everything(), names_to = "Legend",
                     values_to = "y_val") %>%
        arrange(.data$Legend, desc(.data$y_val)) %>%
        mutate(dist = rep(distances, length(labels)))

    } else {
      stop(paste0("Function only applicable for Hazard-rate ",
                  "and Half-normal key functions!"), call. = FALSE)
    }
  }

  #####
  # Re-create the histogram
  #####

  # Detection probability for each fitted value & nhat estimate
  selected <- rep(TRUE, nrow(ds_model$ddf$ds$aux$ddfobj$xmat))
  if (length(ds_model$ddf$fitted) == 1) {
    pdot <- rep(ds_model$ddf$fitted, sum(as.numeric(selected)))
  } else {
    pdot <- ds_model$ddf$fitted[selected]
    nhat <- sum(1 / pdot)
  }

  # Create a dummy histogram

  # Right-truncating

  if (is.null(n_breaks)) {
    if (ds_model$ddf$meta.data$binned) {
      n_breaks <- length(ds_model$ddf$ds$aux$breaks) - 1
    } else {
      n <- length(ds_model$ddf$ds$aux$ddfobj$xmat$distance)
      n_breaks <- round(sqrt(n), 0)
    }
  }

  breaks <- seq(0, trunc, trunc / n_breaks)
  dummy_hist <- hist(dist_data[dist_data$distance <= trunc, ]$distance,
                     breaks = breaks, plot = FALSE)

  # Calculate expected counts for each distance (point transect only)
  nc <- length(breaks) - 1
  expected_counts <- -apply(
    matrix(c(breaks[2:(nc + 1)]^2, breaks[1:nc]^2), ncol = 2, nrow = nc),
    1, diff
  ) * (nhat / breaks[nc + 1]^2)

  # Re-scale the counts
  dummy_hist$counts <- dummy_hist$counts / expected_counts

  #####
  # Calculate the fitted average detection probability
  #####

  ddfobj <- ds_model$ddf$ds$aux$ddfobj
  finebr <- seq(0, trunc, length.out = 101)
  xgrid <- NULL
  linevalues <- NULL
  newdat <- ddfobj$xmat
  for (i in 1:(length(finebr) - 1)) {
    x <- (finebr[i] + finebr[i + 1]) / 2
    xgrid <- c(xgrid, x)
    newdat$distance <- rep(x, nrow(newdat))

    detfct_values <- detfct(newdat$distance, ddfobj, select = selected,
                            width = trunc)
    linevalues <- c(linevalues, sum(detfct_values / pdot) / sum(1 / pdot))
  }

  df_mean <- data.frame(dist = distances, lineval = linevalues)

  #####
  # Make final plot with ggplot()
  #####

  # Use the ggplot2 package to plot the histogram as a barplot and overlay the
  # detection functions
  hist_df <- data.frame(mids = dummy_hist$mids, counts = dummy_hist$counts)

  # Plot with covariates and design matrix
  if (!ds_model$ddf$ds$aux$ddfobj$intercept.only && plot_covariate_fit) {
    palette <- colorRampPalette(
      RColorBrewer::brewer.pal(9, "Set1")
    )(length(labels))

    # Plot with average fitting line
    if (plot_average_fit) {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = .data$mids, y = .data$counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        geom_line(data = df_y_val,
                  aes(x = .data$dist, y = .data$y_val, colour = .data$Legend),
                  linewidth = 1) +
        geom_line(data = df_mean, aes(x = .data$dist, y = .data$lineval),
                  linewidth = 1, linetype = "dashed", colour = "black") +
        labs(x = "distance", y = "detection probability") +
        scale_colour_manual(values = setNames(palette, labels))
      # Plot without average fitting line
    } else {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = .data$mids, y = .data$counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        geom_line(data = df_y_val,
                  aes(x = .data$dist, y = .data$y_val, colour = .data$Legend),
                  linewidth = 1) +
        labs(x = "distance", y = "detection probability") +
        scale_colour_manual(values = setNames(palette, labels))
    }

    # Plot without covariates/design matrix
  } else {
    # Plot average fit
    if (plot_average_fit) {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = .data$mids, y = .data$counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        geom_line(data = df_mean, aes(x = .data$dist, y = .data$lineval),
                  linewidth = 1, linetype = "dashed", colour = "black") +
        labs(x = "distance", y = "detection probability")
      # Plot bar graph
    } else {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = .data$mids, y = .data$counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        labs(x = "distance", y = "detection probability")
    }
  }

  if (show_data) {
    distance_data <- data.frame(
      dist = sort(unique(ds_model$ddf$data$distance))
    )
    out <- out +
      geom_point(
        data = distance_data,
        aes(x = dist, y = rep(0, nrow(distance_data))),
        shape = "|",
        col = "brown",
        size = 3
      )
  }

  # Return plot
  return(out)
}
