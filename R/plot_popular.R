plot_popular <- function(dat, x_var, y_var, col_fill = "#99ff99",
                         col_color = "gray", alpha_val = 0.9){

  stopifnot(is.data.frame(dat))
  stopifnot(is.character(x_var) && is.character(y_var) &&
              is.character(col_fill) &&is.character(col_color))
  stopifnot(is.numeric(alpha_val))
  stopifnot(x_var %in% names(dat))
  stopifnot(y_var %in% names(dat))
  stopifnot(alpha_val >= 0 && alpha_val <= 1)

  dat |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{x_var}},
        y = stats::reorder({{y_var}}, {{x_var}})
      )
    ) +
    ggplot2::geom_col(
      fill = {{col_fill}},
      color = {{col_color}},
      alpha = {{alpha_val}}
    ) +
    ggplot2::labs(
      x = "Number of starts",
      y = ggplot2::element_blank()
    )
}
