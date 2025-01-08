#' Plot popular routes
#'
#' Create a bar plot of the most popular routes.
#'
#' @param dat a data frame with the columns named after the values set for
#' `x_var` and `y_var`
#' @param x_var a column name in `dat` that will be used as the x-axis
#' @param y_var a column name in `dat` that will be used as the y-axis
#' @param col_fill a character string representing the fill color of the bars
#' @param col_color a character string representing the color of the bar edges
#' @param alpha_val a numeric value between 0 and 1 representing the
#' transparency of the bars
#'
#' @returns a ggplot object
#' @export
#'
#' @examples
#' data.frame(n_starts = c(1, 2, 3), route = c("A", "B", "C")) |>
#'   plot_popular(x_var = n_starts, y_var = route)
plot_popular <- function(dat, x_var, y_var, col_fill = "#99ff99",
                         col_color = "gray", alpha_val = 0.9){

  stopifnot(is.data.frame(dat))
  stopifnot(is.character(col_fill) &&is.character(col_color))
  stopifnot(is.numeric(alpha_val))
  stopifnot(x_var |> substitute() |> deparse() %in% names(dat))
  stopifnot(y_var |> substitute() |> deparse() %in% names(dat))
  stopifnot(alpha_val >= 0 && alpha_val <= 1)

  dat |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{x_var}},
        y = stats::reorder({{y_var}}, {{x_var}})
      )
    ) +
    ggplot2::geom_col(
      fill = col_fill,
      color = col_color,
      alpha = alpha_val
    ) +
    ggplot2::labs(
      x = "Number of starts",
      y = ggplot2::element_blank()
    )
}
