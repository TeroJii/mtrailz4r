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
    sort(unique(routestart_dat$route_started_number)),
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
    session_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    event_timestamp2 = c(as.POSIXct("2021-01-01 12:00:00"), as.POSIXct("2021-01-01 12:02:00"), as.POSIXct("2021-01-01 12:04:00"),
                         as.POSIXct("2021-01-01 12:06:00"), as.POSIXct("2021-01-01 12:08:00"), as.POSIXct("2021-01-01 12:10:00"),
                         as.POSIXct("2021-01-01 12:12:00"), as.POSIXct("2021-01-01 12:14:00"), as.POSIXct("2021-01-01 12:16:00"),
                         as.POSIXct("2021-01-01 12:18:00"), as.POSIXct("2021-01-01 12:20:00"))
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


# add breaking tests for issue #8
test_that("route_id get's propagated even if events with different session_id's happen simultaneously", {
  dat <- data.frame(
    row_id = c(1,1, 2,2 ,3,3, 4,4),
    event_name = c("route_started", "route_started", "route_started", "route_started",  "route_stopped", "route_stopped", "route_stopped", "route_stopped"),
    # <dttm>
    event_timestamp2 = c(as.POSIXct("2021-01-01 12:00:00"), as.POSIXct("2021-01-01 12:00:00"), as.POSIXct("2021-01-01 12:10:00"), as.POSIXct("2021-01-01 12:10:00"), as.POSIXct("2021-01-01 12:12:00"), as.POSIXct("2021-01-01 12:12:00"), as.POSIXct("2021-01-01 12:14:00"), as.POSIXct("2021-01-01 12:14:00")),
    session_id = c(1,1, 2,2, 2,2 ,1,1),
    event_params.key = rep(c("route_id", "ga_session_id"), 4),
    event_params.value.string_value = NA,
    # route id 100, session_id according to session_id col
    event_params.value.int_value = c(100, 1, 100, 2, 100, 2, 100, 1)
  )

  lookup_dat <- dat |>
    mt4r_routelookup()

  routestart_dat <- mt4r_routestarts(dat, lookup_dat)

  # this breaks with v0.0.7
  testthat::expect_equal(
    routestart_dat$route_id,
    rep(100, length(routestart_dat$route_id))
  )

  # there should only be one route start per session
  # check that each session_id only contains one unique route_started_number
  starts_dat <- routestart_dat |>
    dplyr::group_by(session_id, route_started_number) |>
    dplyr::summarise(n = dplyr::n()) |>
    # count unique start numbers per session
    dplyr::ungroup() |>
    dplyr::group_by(session_id) |>
    dplyr::count() |>
    dplyr::pull(n)

  # all n should be 1
  testthat::expect_true(
    all(starts_dat == 1)
  )
})
