test_that("non-data.frame returns an error", {
  expect_error(1:3 |> plot_exertime())
})

test_that("missing event_timestamp2 column returns an error", {
  expect_error(mockdata |>
                 mt4r_unnest() |>
                 mt4r_addsessionid() |>
                 plot_exertime())
})

test_that("missing session_id column returns an error", {
  expect_error(data.frame(n_starts = c(1, 2, 3), route = c("A", "B", "C")) |>
                 plot_exertime())
})

test_that("non-character column fill color returns an error", {
  expect_error(mockdata |>
                 mt4r_unnest() |>
                 mt4r_fixtime() |>
                 mt4r_addsessionid() |>
                 plot_exertime(col_fill = 1))
})

test_that("non-character column color returns an error", {
  expect_error(mockdata |>
                 mt4r_unnest() |>
                 mt4r_fixtime() |>
                 mt4r_addsessionid() |>
                 plot_exertime(col_color = 1))
})

test_that("alpha_val above 1 returns an error", {
  expect_error(mockdata |>
                 mt4r_unnest() |>
                 mt4r_fixtime() |>
                 mt4r_addsessionid() |>
                 plot_exertime(alpha_val = 2))
})

test_that("alpha_val below 0 returns an error", {
  expect_error(mockdata |>
                 mt4r_unnest() |>
                 mt4r_fixtime() |>
                 mt4r_addsessionid() |>
                 plot_exertime(alpha_val = -1))
})

test_that("plot_exertime returns a ggplot object", {
  expect_s3_class(mockdata |>
                    mt4r_unnest() |>
                    mt4r_fixtime() |>
                    mt4r_addsessionid() |>
                    plot_exertime(), "gg")
})

test_that("erraneous time_units returns an error", {
  expect_error(mockdata |>
                 mt4r_unnest() |>
                 mt4r_fixtime() |>
                 mt4r_addsessionid() |>
                 plot_exertime(time_units = "days"))
})
