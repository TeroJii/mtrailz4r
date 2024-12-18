
test_that("incorrectly formatted data.frame errors", {
  expect_error(
    object = {
      df <- data.frame(
        x = c(1, 2, 3),
        y = c(4, 5, 6)
      )

      mt4r_unnest(df)
    }
  )
})


test_that("non-data.frame produces an error", {
  expect_error(mt4r_unnest(1:4))
})

test_that("correctly formatted data.frame does not error", {
  expect_silent(mt4r_unnest(mockdata))
})

test_that("correctly formatted data.frame has correct dimensions", {
  expect_equal(
    mt4r_unnest(mockdata) |>
      dim(),
    c(357, 10)
  )
})

test_that("correctly formatted data.frame has correct column names", {
  expect_equal(
    object = {
      mt4r_unnest(mockdata) |>
        colnames()
    },
    expected = {
      c("row_id", "event_date", "event_timestamp", "event_name",
        "event_params.key", "event_params.value.string_value",
        "event_params.value.int_value", "event_params.value.float_value",
        "event_params.value.double_value", "user_pseudo_id")
    }
  )
})
