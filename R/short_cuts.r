.group_across <- function(data, ...) {
  dplyr::group_by(data, dplyr::across(dplyr::any_of(c(...))))
}