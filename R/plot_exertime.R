#' Plot time spent on app
#'
#'  This convenience function plots the time spent on the app per session id.
#'
#' @param dat A data frame with columns `session_id` and `event_timestamp2`
#' @param time_units a character string specifying the time units to use
#' c("mins", "secs", "hours","auto")
#' @param col_fill a character string specifying the fill color
#' @param col_color a character string specifying the color of the histogram bar
#' outlines
#' @param alpha_val transparency of the histogram bars. Numeric value between 0
#' and 1
#'
#' @returns a ggplot object
#' @export
#'
#' @examples
#' mockdata |>
#'   mt4r_unnest() |>
#'   mt4r_addsessionid() |>
#'   mt4r_fixtime() |>
#'   plot_exertime()
plot_exertime <- function(dat, time_units = c("mins", "secs", "hours","auto"),
                          col_fill = "#99ff99",
                          col_color = "gray", alpha_val = 0.9){

  stopifnot(is.data.frame(dat))
  stopifnot(all(c("session_id", "event_timestamp2") %in% names(dat)))
  stopifnot(is.character(col_fill) &&is.character(col_color))
  stopifnot(is.numeric(alpha_val))
  stopifnot(alpha_val >= 0 && alpha_val <= 1)

  time_units <- match.arg(time_units)

  dat |>
    calculate_time_spent(time_units = time_units) |>
    ggplot2::ggplot(ggplot2::aes(x = time_spent)) +
    ggplot2::geom_hline(yintercept = 0, color = "lightgray", linewidth = 0.5) +
    ggplot2::geom_histogram(
      bins = 30,
      fill = col_fill,
      color = col_color,
      alpha = alpha_val
      ) +
    ggplot2::labs(
      title = "Time spent on the app per session id",
      x = paste0("Time spent (", time_units, ")"),
      y = "Number of sessions"
    )
}


calculate_time_spent <- function(dat, time_units) {
  stopifnot(is.data.frame(dat))
  stopifnot(all(c("session_id", "event_timestamp2") %in% names(dat)))

  dat |>
    dplyr::group_by(session_id) |>
    dplyr::summarize(
      min_time = min(event_timestamp2, na.rm = TRUE),
      max_time = max(event_timestamp2, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      time_spent = difftime(
        max_time,
        min_time,
        units = time_units
      ),
      time_units = units(time_spent)
    ) |>
    dplyr::mutate(
      time_spent = as.numeric(time_spent)
    )
}

# Suppress "Undefined global functions or variables" R CMD check note
event_timestamp2 <- time_spent <- max_time <- min_time <- NULL
