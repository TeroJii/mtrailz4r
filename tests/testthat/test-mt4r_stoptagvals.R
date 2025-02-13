test_that("that output is a data.frame", {
  expect_type(object = {
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      mt4r_stoptagvals()
    },
    type = "list")
  expect_s3_class(object = {
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      mt4r_stoptagvals()
    },
    class = "data.frame")
})
