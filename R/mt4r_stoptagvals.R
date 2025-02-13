mt4r_stoptagvals <- function(dat){
  stopifnot(is.data.frame(dat))
  stopifnot("event_name" %in% colnames(dat))
  stopifnot("row_id" %in% colnames(dat))
  stopifnot("event_params.key" %in% colnames(dat))
  stopifnot("event_params.value.int_value" %in% colnames(dat))
  stopifnot("event_params.value.double_value" %in% colnames(dat))


  # get the numeric values for route_id, engagement_time, and route_pois_finished
  # values are gathered for each unique route_stopped event
  route_stop_tag_values <- dat |>
    dplyr::filter(event_name == "route_stopped") |>
    dplyr::group_by(row_id) |>
    dplyr::mutate(
      route_id = dplyr::if_else(event_params.key == "route_id", event_params.value.int_value, NA),
      engagement_time = dplyr::if_else(event_params.key == "engagement_time", event_params.value.double_value, NA),
      route_pois_finished = dplyr::if_else(event_params.key == "route_pois_finished", event_params.value.int_value, NA)
    ) |>
    tidyr::fill(route_id, engagement_time, route_pois_finished, .direction = "updown") |>
    dplyr::ungroup() |>
    dplyr::select(row_id, session_id, event_name, route_id, engagement_time, route_pois_finished) |>
    unique.data.frame()

  return(route_stop_tag_values)
}
