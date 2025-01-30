#' Fix the timestamp column in the data
#'
#' The `event_timestamp` column in the data is a character vector, which
#' represents time passed since the unix epoch in microseconds. This function
#' converts the `event_timestamp` column into a datetime object.
#'
#' @param dat A data frame with the following columns: event_timestamp,
#' event_params.value.int_value, event_params.value.double_value. The input
#' data.frame is created by the [mt4r_unnest()] and modified by the
#' [mt4r_addsessionid()] function.
#'
#' @return A data frame with the `event_timestamp` column converted into a
#'  datetime object (`event_timestamp2`).
#' @export
#'
#' @examples
#' mockdata |>
#'   mt4r_unnest() |>
#'   mt4r_addsessionid() |>
#'   mt4r_fixtime()
mt4r_fixtime <- function(dat){

  stopifnot(is.data.frame(dat))
  # check that appropriate columns are found
  stopifnot(
    "event_timestamp" %in% colnames(dat) &&
      "event_params.value.int_value" %in% colnames(dat) &&
      "event_params.value.double_value" %in% colnames(dat)
  )
  if(!("session_id" %in% colnames(dat))){
    stop("Column with the name 'session_id' not found.
         Please run the mt4r_addsessionid() function first.")
  }


  fixed_dat <- dat |>
    # change character columns into numeric
    dplyr::mutate(
      event_timestamp = as.numeric(event_timestamp),
      event_params.value.int_value = as.numeric(event_params.value.int_value),
      event_params.value.double_value = as.double(
        event_params.value.double_value
      )
    ) |>
    dplyr::mutate(
      event_timestamp2 = lubridate::as_datetime(
        # timestamp is microseconds since the unix epoch
        event_timestamp / (1000*1000)
        ),
      .after = event_timestamp
    )

  return(fixed_dat)
}

# Suppress "Undefined global functions or variables" R CMD check note
event_params.value.double_value <- event_timestamp <- NULL
