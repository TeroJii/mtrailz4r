mt4r_addsessionid <- function(dat){

  stopifnot(is.data.frame(dat))
  stopifnot(
    "event_params.key" %in% colnames(dat) &&
      "event_date" %in% colnames(dat) &&
      "event_params.value.int_value" %in% colnames(dat)
    )
  stopifnot("Column named row_id not found. Please run mt4r_unnest() on your
            data before using mt4r_addsessionid()", "row_id" %in% colnames(dat))

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
