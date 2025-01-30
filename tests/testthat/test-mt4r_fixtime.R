
test_that("incorrectly formatted data.frame errors", {
  expect_error(
    object = {
      df <- data.frame(
        x = c(1, 2, 3),
        y = c(4, 5, 6)
      )

      mt4r_fixtime(df)
      }
    )
})


test_that("non-data.frame produces an error", {
  expect_error(mt4r_fixtime(1:4))
})

test_that("correctly formatted data.frame does not error", {
  expect_silent(object = {
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime()
  })
})


test_that("correctly formatted data.frame producess new timestamp column", {
  expect_true(object = {
    test_dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime()

    "event_timestamp2" %in% colnames(test_dat)
  })
})

test_that("correctly formatted data.frame has correct dimensions", {
  expect_equal(
    mt4r_unnest(mockdata) |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      dim(),
    c(357, 12)
  )
})

test_that("correctly formatted data.frame has correct class", {
  expect_s3_class(
    mt4r_unnest(mockdata) |>
      mt4r_addsessionid() |>
      mt4r_fixtime(),
    "data.frame"
  )
})

test_that("events within session_id's appear in chronological order", {
  expect_true(object = {
    test_dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      #group by sessions
      dplyr::group_by(session_id) |>
      dplyr::filter(!is.na(session_id)) |>
      # test for time differences between events (within session)
      dplyr::mutate(
        time_diff = difftime(
          event_timestamp2,
          lag(event_timestamp2),
          units = "secs"
        )
      ) |>
      # Remove NA values from first rows per session_id
      dplyr::filter(!is.na(time_diff))

    all(test_dat$time_diff >= 0)

  })
})
