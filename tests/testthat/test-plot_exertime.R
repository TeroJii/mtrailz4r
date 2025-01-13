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

test_that("calculate_time_spent returns same amount of observations as the number of session_id's in the input data.frame", {
  expect_equal(object = mockdata |>
                 mt4r_unnest() |>
                 mt4r_fixtime() |>
                 mt4r_addsessionid() |>
                 calculate_time_spent(time_units = "mins") |>
                 nrow(),
               expected = {
                 dat <-  mockdata |>
                   mt4r_unnest() |>
                   mt4r_fixtime() |>
                   mt4r_addsessionid()

                 dat$session_id |>
                   unique() |>
                   length()
                 }
               )
})

test_that("calculate_time_spent errors if time_units is not one of c(\"auto\", \"secs\", \"mins\", \"hours\",\"days\", \"weeks\")", {
  expect_error(mockdata |>
                 mt4r_unnest() |>
                 mt4r_fixtime() |>
                 mt4r_addsessionid() |>
                 calculate_time_spent(time_units = "years")
  )
})

test_that("calculate_time_spent returns expected time differences", {
  test_data <- data.frame(
    session_id = c(1, 1, 2, 2),
    event_timestamp2 = as.POSIXct(c("2021-01-01 00:00:00", "2021-01-01 00:00:30",
                                    "2021-01-01 00:00:00", "2021-01-01 00:01:00"))
    )

  expected_data <- data.frame(
    session_id = c(1, 2),
    min_time = as.POSIXct(c("2021-01-01 00:00:00", "2021-01-01 00:00:00")),
    max_time = as.POSIXct(c("2021-01-01 00:00:30", "2021-01-01 00:01:00"))
  )

  expect_equal(
    object = calculate_time_spent(test_data, time_units = "secs") |>
      as.data.frame(),
    expected = expected_data |>
      dplyr::mutate(
        time_spent = c(30, 60),
        time_units = "secs"
      )
  )

  expect_equal(
    object = calculate_time_spent(test_data, time_units = "mins") |>
      as.data.frame(),
    expected = expected_data |>
      dplyr::mutate(
        time_spent = c(0.5, 1),
        time_units = "mins"
      )
  )

})
