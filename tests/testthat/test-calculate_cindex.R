test_that("C-index gets correct value on reference survival data", {
  data <- yardstick::lung_surv

  reference_value <-
    with(data, surv_obj ~ .pred_time) |>
    survival::concordance() |>
    magrittr::extract2("concordance")

  value <- calculate_cindex(data$.pred_time, data$surv_obj)

  expect_equal(value, reference_value)
})
