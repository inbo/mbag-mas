# nolint start
plot_pdf_per_cat <- function(ds_model) {# nolint: cyclocomp_linter.
  require("Distance")

  # Get formula
  f <- ds_model$ddf$ds$aux$ddfobj$scale$formula

  if (f == "~regio") {
    par(mfrow = c(1, 2))

    plot(ds_model, pdf = TRUE, showpoints = FALSE,
         subset = regio == "De Moeren",
         main = "De Moeren", pl.col = scales::alpha("blue", 0.5))
    add.df.covar.line(ds_model, lwd = 3, lty = 1, col = "blue",
                      data = data.frame(regio = "De Moeren"), pdf = TRUE)

    plot(ds_model, pdf = TRUE, showpoints = FALSE,
         subset = regio == "Oostelijke leemstreek",
         main = "Oostelijke leemstreek",
         pl.col = scales::alpha("darkgreen", 0.5))
    add.df.covar.line(ds_model, lwd = 3, lty = 1, col = "darkgreen",
                      data = data.frame(regio = "Oostelijke leemstreek"),
                      pdf = TRUE)

    par(mfrow = c(1, 1))
  }
  if (f == "~sbp") {
    par(mfrow = c(1, 2))

    plot(ds_model, pdf = TRUE, showpoints = FALSE,
         subset = sbp == "binnen",
         main = "binnen", pl.col = scales::alpha("blue", 0.5))
    add.df.covar.line(ds_model, lwd = 3, lty = 1, col = "blue",
                      data = data.frame(sbp = "binnen"), pdf = TRUE)

    plot(ds_model, pdf = TRUE, showpoints = FALSE,
         subset = sbp == "buiten",
         main = "buiten", pl.col = scales::alpha("darkgreen", 0.5))
    add.df.covar.line(ds_model, lwd = 3, lty = 1, col = "darkgreen",
                      data = data.frame(sbp = "buiten"), pdf = TRUE)

    par(mfrow = c(1, 1))
  }
  if (f == "~openheid") {
    par(mfrow = c(1, 2))

    plot(ds_model, pdf = TRUE, showpoints = FALSE,
         subset = openheid == "OL",
         main = "OL", pl.col = scales::alpha("blue", 0.5))
    add.df.covar.line(ds_model, lwd = 3, lty = 1, col = "blue",
                      data = data.frame(openheid = "OL"), pdf = TRUE)

    plot(ds_model, pdf = TRUE, showpoints = FALSE,
         subset = openheid == "HOL",
         main = "HOL", pl.col = scales::alpha("darkgreen", 0.5))
    add.df.covar.line(ds_model, lwd = 3, lty = 1, col = "darkgreen",
                      data = data.frame(openheid = "HOL"), pdf = TRUE)

    par(mfrow = c(1, 1))
  }
  if (f %in% c("~regio + sbp", "~regio * sbp")) {
    par(mfrow = c(1, 2))

    cols <- c("blue", "darkgreen")


    for (i in seq_along(unique(ds_model$ddf$data$sbp))) {
      j <- sort(unique(ds_model$ddf$data$sbp))[i]

      for (k in seq_along(unique(ds_model$ddf$data$regio))) {
        reg <- sort(unique(ds_model$ddf$data$regio))[k]

        plot(ds_model, pdf = TRUE, showpoints = FALSE,
             subset = regio == reg & sbp == j,
             main = paste(reg, j, sep = "- "),
             pl.col = scales::alpha(cols[i], 0.5))
        add.df.covar.line(ds_model, lwd = 3, lty = 1, col = cols[i],
                          data = data.frame(regio = reg, sbp = j),
                          pdf = TRUE)
      }
    }

    par(mfrow = c(1, 1))
  }
  if (f %in% c("~regio + openheid", "~regio * openheid")) {
    par(mfrow = c(1, 2))

    cols <- c("blue", "darkgreen")


    for (i in seq_along(unique(ds_model$ddf$data$openheid))) {
      j <- sort(unique(ds_model$ddf$data$openheid))[i]

      for (k in seq_along(unique(ds_model$ddf$data$regio))) {
        reg <- sort(unique(ds_model$ddf$data$regio))[k]

        plot(ds_model, pdf = TRUE, showpoints = FALSE,
             subset = regio == reg & openheid == j,
             main = paste(reg, j, sep = "- "),
             pl.col = scales::alpha(cols[i], 0.5))
        add.df.covar.line(ds_model, lwd = 3, lty = 1, col = cols[i],
                          data = data.frame(regio = reg, openheid = j),
                          pdf = TRUE)
      }
    }

    par(mfrow = c(1, 1))
  }
  if (f %in% c("~sbp + openheid", "~sbp * openheid")) {
    par(mfrow = c(1, 2))

    cols <- c("blue", "darkgreen")


    for (i in seq_along(unique(ds_model$ddf$data$openheid))) {
      j <- sort(unique(ds_model$ddf$data$openheid))[i]

      for (k in seq_along(unique(ds_model$ddf$data$sbp))) {
        reg <- sort(unique(ds_model$ddf$data$sbp))[k]

        plot(ds_model, pdf = TRUE, showpoints = FALSE,
             subset = sbp == reg & openheid == j,
             main = paste(reg, j, sep = "- "),
             pl.col = scales::alpha(cols[i], 0.5))
        add.df.covar.line(ds_model, lwd = 3, lty = 1, col = cols[i],
                          data = data.frame(sbp = reg, openheid = j),
                          pdf = TRUE)
      }
    }

    par(mfrow = c(1, 1))
  }
  if (f %in% c("~regio + sbp + openheid", "regio+sbp*openheid")) {
    par(mfrow = c(1, 2))

    cols <- c("blue", "darkgreen")

    for (h in seq_along(unique(ds_model$ddf$data$openheid))) {
      o <- sort(unique(ds_model$ddf$data$openheid))[h]

      for (i in seq_along(unique(ds_model$ddf$data$sbp))) {
        j <- sort(unique(ds_model$ddf$data$sbp))[i]

        for (k in seq_along(unique(ds_model$ddf$data$regio))) {
          reg <- sort(unique(ds_model$ddf$data$regio))[k]

          plot(ds_model, pdf = TRUE, showpoints = FALSE,
               subset = regio == reg & sbp == j & openheid == o,
               main = paste(reg, j, o, sep = "- "),
               pl.col = scales::alpha(cols[h], 0.5))
          add.df.covar.line(ds_model, lwd = 3, lty = 1, col = cols[h],
                            data = data.frame(regio = reg, sbp = j,
                                              openheid = o),
                            pdf = TRUE)
        }
      }
    }
    par(mfrow = c(1, 1))
  }
  if (f == "~stratum") {
    par(mfrow = c(1, 2))

    cols <- c("blue", "darkgreen")
    for (s in seq_along(unique(ds_model$ddf$data$stratum))) {
      strat <- sort(unique(ds_model$ddf$data$sbp))[s]

      plot(ds_model, pdf = TRUE, showpoints = FALSE,
           subset = stratum == strat,
           main = strat, pl.col = scales::alpha(cols[s %% 2 + 1], 0.5))
      add.df.covar.line(ds_model, lwd = 3, lty = 1, col = cols[s %% 2 + 1],
                        data = data.frame(stratum = strat), pdf = TRUE)
    }

    par(mfrow = c(1, 1))
  }
}
# nolint end
