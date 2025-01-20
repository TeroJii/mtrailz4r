test_that("missing or erraneous lookup data produces an error", {
  dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_fixtime() |>
    mt4r_addsessionid()

  expect_error(mt4r_routestarts(dat))
  expect_error(mt4r_routestarts(dat, dat))
  expect_error(mt4r_routestarts(dat, NULL))
  expect_error(mt4r_routestarts(dat, mtcars))
})

test_that("missing or erraneous data produces an error", {
  dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_fixtime() |>
    mt4r_addsessionid()

  lookup_dat <- dat |>
    mt4r_routelookup()

  expect_error({
    dat <- dat[, -which(names(dat) == "row_id")]

    mt4r_routestarts(dat, lookup_dat)
  })
  expect_error(mt4r_routestarts(NULL, lookup_dat))
  expect_error({mt4r_routestarts(mtcars, lookup_dat)})
  expect_error({mt4r_routestarts(1:5, lookup_dat)})
})

test_that("missing or erraneous data filtering parameter produces an error", {
  dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_fixtime() |>
    mt4r_addsessionid()

  lookup_dat <- dat |>
    mt4r_routelookup()

  expect_error(mt4r_routestarts(dat, lookup_dat, "TRUE"))
  expect_error(mt4r_routestarts(dat, lookup_dat, 1))
  expect_error(mt4r_routestarts(dat, lookup_dat, NULL))
  expect_error(mt4r_routestarts(dat, lookup_dat, mtcars))
})

test_that("function output is a tibble", {
  dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_fixtime() |>
    mt4r_addsessionid()

  lookup_dat <- dat |>
    mt4r_routelookup()

  routestart_dat <- mt4r_routestarts(dat, lookup_dat)

  expect_s3_class(routestart_dat, "tbl_df")
})

test_that("lookup data contains same route start numbers as output data", {
  dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_fixtime() |>
    mt4r_addsessionid()

  lookup_dat <- dat |>
    mt4r_routelookup()

  routestart_dat <- mt4r_routestarts(dat, lookup_dat, filter_zero_data = FALSE)

  expect_equal(
    unique(routestart_dat$route_started_number),
    unique(lookup_dat$route_started_number)
  )
})
