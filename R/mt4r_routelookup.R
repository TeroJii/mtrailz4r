mt4r_routelookup <- function(dat){

  stopifnot(is.data.frame(dat))
  stopifnot("row_id" %in% names(dat))
  stopifnot("event_name" %in% names(dat))
  stopifnot("event_timestamp2" %in% names(dat))

  lookup_dat <- dat |>
    dplyr::group_by(row_id) |>
    dplyr::mutate(
      has_route_started = any(event_name == "route_started"),
      .after = event_timestamp2
    ) |>
    dplyr::summarise(
      has_route_started = mean(has_route_started)
    ) |>
    dplyr::mutate(route_started_number = cumsum(has_route_started)) |>
    dplyr::ungroup()

  return(lookup_dat)
}
