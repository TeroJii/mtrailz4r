test_that("erraneously formatted input creates as error", {
  expect_error(1:5 |> mt4r_routelookup())
  expect_error(mtcars |> mt4r_routelookup())
  expect_error({
    input_dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime()

    # de-select row_id
    input_dat <- input_dat[, -which(names(input_dat) == "row_id")]

    input_dat |> mt4r_routelookup()
  })
})


test_that("using mockdata as input creates a non-null output",{
  expect_gt(mockdata |>
              mt4r_unnest() |>
              mt4r_addsessionid() |>
              mt4r_fixtime() |>
              mt4r_routelookup() |>
              nrow(),
            0
  )
})


test_that("maximum route_started_number is smaller (or equal to) than number of unique rows",{
  expect_lte({
    lookup_dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      mt4r_routelookup()

    max(lookup_dat$route_started_number)
    }, nrow(lookup_dat)
  )
})

test_that("route_started_number is a non-negative integer",{
  expect_equal({
    lookup_dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      mt4r_routelookup()

    # check that no negative numbers are present
    sum(lookup_dat$route_started_number < 0)
    }, 0
  )
  expect_equal({
    lookup_dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      mt4r_routelookup()

    # check that no non-integer numbers are present
    sum(!lookup_dat$route_started_number %% 1 == 0)
    }, 0
  )
})

test_that("output is a tibble", {
  expect_s3_class(mockdata |>
                    mt4r_unnest() |>
                    mt4r_addsessionid() |>
                    mt4r_fixtime() |>
                    mt4r_routelookup(),
                  "tbl_df"
  )
})

test_that("route_start_number is a running number", {
  lookup_dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_addsessionid() |>
    mt4r_fixtime() |>
    mt4r_routelookup()

  expect_equal(unique(lookup_dat$route_started_number), 0:max(lookup_dat$route_started_number))
})

test_that("the maximum route_started_number equals the amount of route starts in data", {
  lookup_dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_addsessionid() |>
    mt4r_fixtime() |>
    mt4r_routelookup()

  ref_dat <- mockdata |>
    mt4r_unnest() |>
    mt4r_addsessionid() |>
    mt4r_fixtime()

  num_unique_route_starts <- ref_dat |>
    dplyr::group_by(row_id) |>
    dplyr::summarise(
      has_route_start = any(event_name == "route_started")
    ) |>
    dplyr::pull(has_route_start) |>
    sum()

  expect_equal(max(lookup_dat$route_started_number), num_unique_route_starts)
})
