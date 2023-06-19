# These imports are valid for the entire package, so where we declare them
# doesn’t really matter, as long as it’s somewhere where roxygen2 finds them.
#' @importFrom dplyr %>% .data between count filter group_by mutate pull slice_max summarize
#' @importFrom tidyr pivot_wider
#' @importFrom grDevices boxplot.stats
#' @importFrom stats setNames
#' @import ggplot2
#' @import shiny
'_PACKAGE'

days_per_year = 365L

.onLoad = function (libname, pkgname) {
  load_data()
  setup_theme()
}

load_data = function () {
  assign(
    'all_tumor_stages',
    sort(unique(dataset$tumor_stage)),
    envir = topenv()
  )

  assign(
    'all_ages',
    range(as.integer(dataset$age_at_diagnosis), na.rm = TRUE) %/% days_per_year,
    envir = topenv()
  )

  assign(
    'all_races',
    levels(stats::relevel(factor(sort(unique(dataset$race))), 'not reported')),
    envir = topenv()
  )
}

setup_theme = function () {
  # Set a white plot background to avoid clashing with the foreground colors.
  theme_set(theme_minimal())
}
