test_that("non-data.frame returns an error", {
  expect_error(1:3 |> plot_popular(x_var = 1, y_var = 2))
})

test_that("x_var not present returns an error", {
  expect_error(data.frame(n_starts = c(1, 2, 3), route = c("A", "B", "C")) |>
                 plot_popular(x_var = test_name, y_var = route, col_fill = 1))
})

test_that("y_var not present returns an error", {
  expect_error(data.frame(n_starts = c(1, 2, 3), route = c("A", "B", "C")) |>
                 plot_popular(x_var = n_starts, y_var = test_name, col_fill = 1))
})

test_that("non-character column fill color returns an error", {
  expect_error(data.frame(n_starts = c(1, 2, 3), route = c("A", "B", "C")) |>
                 plot_popular(x_var = n_starts, y_var = route, col_fill = 1))
})

test_that("non-character column color returns an error", {
  expect_error(data.frame(n_starts = c(1, 2, 3), route = c("A", "B", "C")) |>
                 plot_popular(x_var = n_starts, y_var = route, col_color = 1))
})

test_that("alpha_val above 1 returns an error", {
  expect_error(data.frame(n_starts = c(1, 2, 3), route = c("A", "B", "C")) |>
                 plot_popular(x_var = n_starts, y_var = route, alpha_val = 2))
})

test_that("alpha_val below 0 returns an error", {
  expect_error(data.frame(n_starts = c(1, 2, 3), route = c("A", "B", "C")) |>
                 plot_popular(x_var = n_starts, y_var = route, alpha_val = -1))
})

test_that("plot_popular returns a ggplot object", {
  expect_s3_class(data.frame(n_starts = c(1, 2, 3), route = c("A", "B", "C")) |>
                    plot_popular(x_var = n_starts, y_var = route), "gg")
})
