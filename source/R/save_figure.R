save_figure <- function(plot, file, devices, ...) {
  # Remove extension if provided
  file <- gsub("\\.([[:alnum:]]*)$", "", file)
  # Add and save all extensions provided in devices argument
  sapply(devices, function(dev) {
    ggplot2::ggsave(
      filename = paste(file, dev, sep = "."),
      plot = plot,
      ...
    )
  })
}
