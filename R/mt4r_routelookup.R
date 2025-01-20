#' Creating a lookup table for route starts
#'
#' This function creates a lookup table for route starts. It takes a data frame
#' and returns a data frame with a running number for each route start.
#'
#' @param dat A data frame with columns row_id, and event_name. The input
#'  data.frame is created by the [mt4r_unnest()], [mt4r_fixtime()], and
#'  mt4r_addsessionid() functions.
#'
#'
#' @returns A data frame with columns row_id, has_route_started, and
#' route_started_number
#' @export
#'
#' @examples
#' mockdata |>
#'   mt4r_unnest() |>
#'   mt4r_fixtime() |>
#'   mt4r_addsessionid() |>
#'   mt4r_routelookup()
mt4r_routelookup <- function(dat){

  stopifnot(is.data.frame(dat))
  stopifnot("row_id" %in% names(dat))
  stopifnot("event_name" %in% names(dat))

  lookup_dat <- dat |>
    dplyr::group_by(row_id) |>
    dplyr::mutate(has_route_started = any(event_name == "route_started")) |>
    dplyr::summarise(
      has_route_started = mean(has_route_started)
    ) |>
    dplyr::mutate(route_started_number = cumsum(has_route_started)) |>
    dplyr::ungroup()

  return(lookup_dat)
}

# Suppress "Undefined global functions or variables" R CMD check note
event_name <- has_route_started <- NULL
