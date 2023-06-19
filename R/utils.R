order_by_count = function (x, wt = NULL, descending = FALSE) {
  if (is.null(wt)) {
    forcats::fct_reorder(x, x, .fun = length, .desc = descending)
  } else {
    forcats::fct_reorder(x, wt, .fun = sum, .desc = descending)
  }
}

if_unselected = function (x, default) {
  if (length(x) == 0L) default else x
}
