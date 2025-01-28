#' Add route name and route start numbers to data
#'
#' This function adds a running route start number to the data. Additionally,
#' a `route_name` column is added to the data, to help easily connect route
#' starts to specific routes.
#'
#' @param dat A data frame with the following columns: row_id, session_id,
#' event_name, event_params.key, event_params.value.string_value. The input
#' data.frame is created by the [mt4r_unnest()], [mt4r_fixtime()], and
#' [mt4r_addsessionid()] functions.
#' @param route_lookup A lookup  data frame with the running route start
#' numbers. Is created by the [mt4r_routelookup()] function.
#' @param filter_zero_data A logical value. If TRUE, rows where a route has not
#' started are removed from the data.
#'
#' @returns A data frame with the `route_started_number`, `route_id` and
#' `route_name` columns added to the input data.
#' @export
#'
#' @examples
#' dat <- mockdata |>
#'   mt4r_unnest() |>
#'   mt4r_fixtime() |>
#'   mt4r_addsessionid()
#'
#'  lookup_dat <- dat |>
#'   mt4r_routelookup()
#'
#'  dat |>
#'    mt4r_routestarts(lookup_dat)
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
    dplyr::mutate(
      # get the route name per route_started event
      route_name = dplyr::if_else(
        event_name == "route_started" &
          event_params.key == "route_name",
        true = event_params.value.string_value,
        false = NA_character_),
      # get the route_id per route_started event
      route_id = dplyr::if_else(
        event_name == "route_started" &
          event_params.key == "route_id",
        true = event_params.value.int_value,
        false = NA)
    ) |>
    # populate route_name to all rows in the session
    dplyr::group_by(session_id, route_started_number) |>
    tidyr::fill(route_name, route_id, .direction = "updown") |>
    dplyr::ungroup()

  return(routestart_dat)
}

# Suppress "Undefined global functions or variables" R CMD check note
route_started_number <- event_params.value.string_value <- route_name <-  NULL
