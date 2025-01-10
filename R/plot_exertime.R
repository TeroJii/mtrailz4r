plot_exertime <- function(dat, time_units = "mins", col_fill = "#99ff99",
                          col_color = "gray", alpha_val = 0.9){

  stopifnot(is.data.frame(dat))
  stopifnot(is.character(col_fill) &&is.character(col_color))
  stopifnot(is.numeric(alpha_val))
  stopifnot(alpha_val >= 0 && alpha_val <= 1)

  dat |>
    dplyr::group_by(session_id) |>
    dplyr::mutate(
      time_spent = difftime(
        max(event_timestamp2, na.rm = TRUE),
        min(event_timestamp2, na.rm = TRUE),
        units = time_units
      )
    ) |>
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
