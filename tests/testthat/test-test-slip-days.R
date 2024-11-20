test_that("order assignments - correct order", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c("2/15/2024 11:11:11", "2/15/2024 05:11:11", "2/15/2024 11:59:59")
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 1", "Lab 2")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 2"))
})

test_that("order assignments - reverse order", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c("2/15/2024 11:11:11", "2/15/2024 05:11:11", "2/15/2024 11:59:59")
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 2", "Lab 1")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 2"))
})

test_that("order assignments - same time submission, keep gs order", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59")
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 2", "Lab 1")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 2"))
})

test_that("order assignments - one missing submission time, add last", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c(NA, NA, NA),
    `Lab 3 - Submission Time` = c("2/15/2024 11:11:11", "2/15/2024 05:11:11", "2/15/2024 11:59:59")
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 1", "Lab 2", "Lab 3")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 3", "Lab 2"))
})

test_that("order assignments - two missing submission time, add last", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c(NA, NA, NA),
    `Lab 3 - Submission Time` = c("2/15/2024 11:11:11", "2/15/2024 05:11:11", "2/15/2024 11:59:59"),
    `Lab 4 - Submission Time` = c(NA, NA, NA),
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 3", "Lab 2", "Lab 4"))
})


test_that("order assignments - all missing submission time, add in alpha order", {
  gs <- tibble::tibble(
    `Lab 3 - Submission Time` = c(NA, NA, NA),
    `Lab 2 - Submission Time` = c(NA, NA, NA),
    `Lab 1 - Submission Time` = c(NA, NA, NA)
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 2", "Lab 1", "Lab 3")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 2", "Lab 3"))
})
