#' Unnesting json data
#'
#' Data exported from Firebase is in json format. This function unnests the
#' json data.
#'
#' @param dat A data frame with columns `event_params`, `event_date`, and
#' `user_properties`. Created by reading in the exported data from Firebase.
#'
#' @return An unnested data frame with `user_properties` column removed.
#' @export
#'
#' @examples
#' # Example data before and after unnesting
#' mockdata |>
#'  dim()
#'
#' mockdata |>
#'   mt4r_unnest() |>
#'   dim()
mt4r_unnest <- function(dat){

  stopifnot(is.data.frame(dat))
  stopifnot("event_params" %in% colnames(dat) && "event_date" %in% colnames(dat) && "user_properties" %in% colnames(dat))

  # Unnesting json data
  unnested <- dat |>
    dplyr::mutate(row_id = dplyr::row_number(), .before = event_date) |>
    dplyr::select(-user_properties) |>
    tidyr::unnest(cols = c(event_params), keep_empty = TRUE, names_sep = ".") |>
    tidyr::unnest(cols = c(event_params.value), keep_empty = TRUE, names_sep = ".")

  return(unnested)
}

# Supress "Undefined global functions or variables" R CMD check note
event_date <- event_params <- user_properties <- event_params.value <- NULL
