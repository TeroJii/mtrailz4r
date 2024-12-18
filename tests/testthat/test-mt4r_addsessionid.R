
test_that("incorrectly formatted data.frame errors", {
  expect_error(
    object = {
      df <- data.frame(
        x = c(1, 2, 3),
        y = c(4, 5, 6)
      )
      mt4r_addsessionid(df)
    }
  )
})


test_that("non-data.frame produces an error", {
  expect_error(mt4r_addsessionid(1:4))
})

test_that("correctly formatted data.frame does not error", {
  expect_silent(object = {
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid()
  })
})

test_that("Session id does not contain NA values", {
  expect_false(object = {
    test_dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid()

    anyNA(test_dat$session_id)
  }
  )
})

test_that("We get an error if session_id is already present", {
  expect_warning(object = {
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_addsessionid()
  }
  )
})
