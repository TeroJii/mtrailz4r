#' Add session_id column
#'
#' This function adds a session_id column to the data.
#' The session_id is a unique identifier for each session.
#'
#' @param dat A data frame with the following columns: event_params.key,
#' event_date, event_params.value.int_value, row_id. The input data.frame is
#' created by the [mt4r_unnest()] function.
#'
#' @return A data frame with the session_id column added.
#' @export
#'
#' @examples
#' mockdata |>
#'   mt4r_unnest() |>
#'   mt4r_addsessionid()
mt4r_addsessionid <- function(dat){

  stopifnot(is.data.frame(dat))
  stopifnot(
    "event_params.key" %in% colnames(dat) &&
      "event_date" %in% colnames(dat) &&
      "event_params.value.int_value" %in% colnames(dat)
    )
  stopifnot("row_id" %in% colnames(dat))
  #warn if session_id already exists
  if("session_id" %in% colnames(dat)){
    warning("Column with the name 'session_id' already exists.
            Function will overwrite the existing column.")
  }

  # Adding session_id
  has_session_id <- dat |>
    dplyr::mutate(
      session_id = ifelse(
        event_params.key == "ga_session_id",
        event_params.value.int_value,
        NA
        ),
      .after = event_date
    ) |>
    dplyr::group_by(row_id) |>
    tidyr::fill(session_id, .direction = "updown") |>
    dplyr::ungroup()

  return(has_session_id)
}

# Suppress "Undefined global functions or variables" R CMD check note
event_params.key <- event_params.value.int_value <- row_id <- session_id <- NULL
