plot_popular <- function(dat, x_var, y_var, col_fill = "#99ff99",
                         col_color = "gray", alpha_val = 0.9){

  dat |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{x_var}},
        y = reorder({{y_var}}, {{x_var}})
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
