mt4r_fixtime <- function(dat){

  stopifnot(is.data.frame(dat))
  # check that appropriate columns are found
  stopifnot(
    "event_timestamp" %in% colnames(dat) &&
      "event_params.value.int_value" %in% colnames(dat) &&
      "event_params.value.double_value" %in% colnames(dat)
  )


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
        event_timestamp / (1000*1000)
        ),
      .after = event_timestamp
    )

  return(fixed_dat)
}
