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


test_that("function throws error if correct column names are not present", {
  expect_error(
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      dplyr::select(-row_id) |>
      mt4r_stoptagvals()
  )
  expect_error(
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      dplyr::select(-event_name) |>
      mt4r_stoptagvals()
  )
  expect_error(
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      dplyr::select(-event_params.key) |>
      mt4r_stoptagvals()
  )
  expect_error(
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      dplyr::select(-event_params.value.int_value) |>
      mt4r_stoptagvals()
  )
  expect_error(
    mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      dplyr::select(-event_params.value.double_value) |>
      mt4r_stoptagvals()
  )
  expect_error(
    mtcars |> mt4r_stoptagvals()
  )
})

test_that("A warning is issued if route_id is missing", {
  expect_warning(
    {dat <- mockdata |>
      mt4r_unnest() |>
      mt4r_addsessionid() |>
      mt4r_fixtime() |>
      dplyr::mutate(event_params.key = dplyr::if_else(event_params.key == "route_id", NA, event_params.key))


    dat |>
      mt4r_stoptagvals()
    })
})
