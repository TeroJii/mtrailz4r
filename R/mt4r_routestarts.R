mt4r_routestarts <- function(dat, route_lookup, filter_zero_data = TRUE){

  stopifnot(is.data.frame(dat))
  stopifnot(is.data.frame(route_lookup))
  stopifnot("row_id" %in% names(dat))
  stopifnot("route_started_number" %in% names(route_lookup))
  stopifnot(is.logical(filter_zero_data))

  routestart_dat <- dat |>
    dplyr::left_join(route_lookup, by = "row_id")

  if(filter_zero_data){
    # remove rows where a route has not started
    routestart_dat <- routestart_dat |>
      dplyr::filter(route_started_number > 0)
  }

  routestart_dat <- routestart_dat |>
    # get the route name per route_started event
    dplyr::mutate(
      route_name = dplyr::if_else(
        event_name == "route_started" &
          event_params.key == "route_name",
        true = event_params.value.string_value,
        false = NA_character_)
    ) |>
    # populate route_name to all rows in the session
    dplyr::group_by(session_id, route_started_number) |>
    tidyr::fill(route_name, .direction = "updown") |>
    dplyr::ungroup()

  return(routestart_dat)
}
