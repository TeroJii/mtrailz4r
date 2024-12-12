df <- data.frame(
  x = c(1, 2, 3),
  y = c(4, 5, 6)
)

test_that("incorrectly formatted data.frame errors", {
  expect_error(mt4r_fixtime(df))
})


test_that("non-data.frame produces an error", {
  expect_error(mt4r_fixtime(1:4))
})
