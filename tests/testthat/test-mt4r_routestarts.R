test_that("missing or erraneous lookup data produces an error", {
  dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_addsessionid() |>
    mt4r_fixtime()

  expect_error(mt4r_routestarts(dat))
  expect_error(mt4r_routestarts(dat, dat))
  expect_error(mt4r_routestarts(dat, NULL))
  expect_error(mt4r_routestarts(dat, mtcars))
})

test_that("missing or erraneous data produces an error", {
  dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_addsessionid() |>
    mt4r_fixtime()

  lookup_dat <- dat |>
    mt4r_routelookup()

  expect_error({
    dat <- dat[, -which(names(dat) == "row_id")]

    mt4r_routestarts(dat, lookup_dat)
  })
  expect_error(mt4r_routestarts(NULL, lookup_dat))
  expect_error({mt4r_routestarts(mtcars, lookup_dat)})
  expect_error({mt4r_routestarts(1:5, lookup_dat)})
  expect_error({
    dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime()

    dat <- dat[, -which(names(dat) == "event_name")]

    mt4r_routestarts(dat, lookup_dat)
  })
  expect_error({
    dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime()

    dat <- dat[, -which(names(dat) == "event_params.key")]

    mt4r_routestarts(dat, lookup_dat)
  })
  expect_error({
    dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime()

    dat <- dat[, -which(names(dat) == "event_params.value.string_value")]

    mt4r_routestarts(dat, lookup_dat)
  })
  expect_error({
    dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime()

    dat <- dat[, -which(names(dat) == "event_params.value.int_value")]

    mt4r_routestarts(dat, lookup_dat)
  })
  expect_error({
    dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime()

    dat <- dat[, -which(names(dat) == "session_id")]

    mt4r_routestarts(dat, lookup_dat)
  })
})

test_that("missing or erraneous data filtering parameter produces an error", {
  dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_addsessionid() |>
    mt4r_fixtime()

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
    mt4r_addsessionid() |>
    mt4r_fixtime()

  lookup_dat <- dat |>
    mt4r_routelookup()

  routestart_dat <- mt4r_routestarts(dat, lookup_dat)

  expect_s3_class(routestart_dat, "tbl_df")
})

test_that("lookup data contains same route start numbers as output data", {
  dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_addsessionid() |>
    mt4r_fixtime()

  lookup_dat <- dat |>
    mt4r_routelookup()

  routestart_dat <- mt4r_routestarts(dat, lookup_dat, filter_zero_data = FALSE)

  expect_equal(
    unique(routestart_dat$route_started_number),
    unique(lookup_dat$route_started_number)
  )
})


test_that("test data returns sane outputs", {
  test_data <- data.frame(
    row_id = 1:11,
    event_name = c("route_started", "route_stopped", "wut", "route_started", "route_stopped",
                   "route_started", "route_stopped", "route_started", "route_stopped",
                   "route_started", "route_stopped"),
    event_params.key = c(rep("route_name", 2), "", rep("route_name", 8)),
    event_params.value.string_value = c("A", "A", "A", "B", "B", "C", "C", "D", "D", "E", "E"),
    event_params.value.int_value = c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
    session_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )

  lookup_dat <- test_data |>
    mt4r_routelookup()

  expect_s3_class(test_data |>
                    mt4r_routestarts(route_lookup = lookup_dat),
                  "tbl_df"
  )
  # route starts with test data equal the number of route_started tags
  expect_equal(
    test_data |>
      mt4r_routestarts(route_lookup = lookup_dat) |>
      dplyr::pull(route_started_number) |>
      max(),
    sum(test_data$event_name == "route_started"))
})
