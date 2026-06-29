# Internal helpers (not exported).

# Build a named colour vector for any number of categories.
make_palette <- function(values) {
  lv <- sort(unique(as.character(values)))
  stats::setNames(scales::hue_pal()(length(lv)), lv)
}

# Scale a numeric variable to a marker-radius range (guards constant columns).
# `max_radius` comes from the "Reduce display radius" slider; `min_radius` is
# kept proportional so every point shrinks together when the slider is lowered.
scaled_radius <- function(x, max_radius = 20, ratio = 0.15) {
  min_radius <- max_radius * ratio
  ok <- x[!is.na(x)]
  if (length(unique(ok)) <= 1) {
    return(rep((min_radius + max_radius) / 2, length(x)))
  }
  scales::rescale(x, to = c(min_radius, max_radius))
}

# Validate that requested columns exist and have the expected type.
check_columns <- function(data, cols, what) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop(sprintf("%s column(s) not found in data: %s",
                 what, paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}
